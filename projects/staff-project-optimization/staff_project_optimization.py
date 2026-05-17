import os
import sys
from collections import defaultdict
from pathlib import Path

os.environ.setdefault("MPLCONFIGDIR", "/private/tmp/matplotlib")

import pulp
import requests
from matplotlib.backends.backend_qtagg import FigureCanvasQTAgg as FigureCanvas
from matplotlib.figure import Figure
from PyQt6 import uic
from PyQt6.QtWidgets import QApplication, QMainWindow, QVBoxLayout


BASE_URL = "https://bit-coursework-api.azurewebsites.net/project"
MAX_WEEKLY_HOURS = 40
MAX_TOTAL_HOURS = 480
UI_FILE = Path(__file__).with_name("FinalProject2.ui")


def get_json(endpoint):
    response = requests.get(f"{BASE_URL}/{endpoint}", timeout=30)
    response.raise_for_status()
    return response.json()


def try_get_json(endpoint):
    try:
        return get_json(endpoint)
    except requests.RequestException:
        return None


def value_from(record, *names, default=None):
    lower_record = {}
    for key, value in record.items():
        lower_record[key.lower()] = value

    for name in names:
        if name in record:
            return record[name]
        value = lower_record.get(name.lower())
        if value is not None:
            return value

    return default


def normalize_id(value):
    try:
        return int(value)
    except (TypeError, ValueError):
        return value


def id_from(record, *names):
    value = value_from(record, *names)
    if isinstance(value, dict):
        value = value_from(value, "id", "ID", "classificationID", "staffID", "projectID")
    return normalize_id(value)


def money(value):
    return f"${value:,.2f}"


def load_api_data():
    staff_data = try_get_json("Staff")
    if staff_data is None:
        staff_data = []
        for staff_id in range(1, 21):
            staff_data.append(get_json(f"Staff/{staff_id}"))

    staff_rates_data = get_json("StaffRates")
    return staff_data, staff_rates_data


def get_project_hours(projects, classifications):
    all_project_hours = try_get_json("ProjectHours")
    if all_project_hours is not None:
        return all_project_hours

    project_hours = []
    for project_id in projects:
        for classification_id in classifications:
            rows = get_json(f"ProjectHours/{project_id}/{classification_id}")
            for row in rows:
                row["projectId"] = value_from(row, "projectId", "ProjectId", default=project_id)
                row["classificationId"] = value_from(
                    row,
                    "classificationId",
                    "ClassificationId",
                    "classification",
                    "Classification",
                    default=classification_id,
                )
                project_hours.append(row)
    return project_hours


def prepare_data():
    staff_data, staff_rates_data = load_api_data()

    staff = {}
    projects = {}
    classifications = {}
    staff_project_rate = {}

    for staff_row in staff_data:
        staff_id = id_from(staff_row, "staffId", "StaffId", "staffID", "id", "Id")
        classification_id = id_from(
            staff_row,
            "classificationId",
            "ClassificationId",
            "staffClassificationID",
            "classification",
            "Classification",
            "staffClassification",
        )
        first_name = value_from(staff_row, "firstName", "FirstName", default="")
        last_name = value_from(staff_row, "lastName", "LastName", default="")
        base_rate = float(value_from(staff_row, "baseRate", "BaseRate", "salary", "Salary", default=0))
        classification_row = value_from(staff_row, "staffClassification", "StaffClassification", default={})
        classification_name = value_from(
            classification_row,
            "classification",
            "Classification",
            default=f"Classification {classification_id}",
        )

        staff[staff_id] = {
            "id": staff_id,
            "name": f"{first_name} {last_name}".strip(),
            "classification_id": classification_id,
            "classification": classification_name,
            "base_rate": base_rate,
        }
        classifications[classification_id] = classification_name

    for rate_row in staff_rates_data:
        staff_id = id_from(rate_row, "staffId", "StaffId", "staffID", "staff")
        project_id = id_from(rate_row, "projectId", "ProjectId", "projectID", "project")
        staff_project_rate[(staff_id, project_id)] = float(
            value_from(rate_row, "rate", "Rate", "hourlyRate", "HourlyRate", default=0)
        )

        project_row = value_from(rate_row, "project", "Project", default={})
        projects[project_id] = {
            "id": project_id,
            "name": value_from(
                project_row,
                "companyName",
                "CompanyName",
                "name",
                "Name",
                default=f"Project {project_id}",
            ),
            "industry": value_from(project_row, "industry", "Industry", default="Not Available"),
            "revenue": float(value_from(project_row, "revenue", "Revenue", default=0)),
        }

    staff_ids = sorted(staff)
    project_ids = sorted(projects)
    classification_ids = sorted(classifications)
    project_hours_data = get_project_hours(project_ids, classification_ids)
    weeks = sorted(
        {
            id_from(hour_row, "week", "Week", "weekId", "WeekId", "weekNum", "WeekNum")
            for hour_row in project_hours_data
        }
    )

    hours_by_class_project_week = {}
    for hour_row in project_hours_data:
        project_id = id_from(hour_row, "projectId", "ProjectId", "projectID", "project", "Project")
        classification_id = id_from(
            hour_row,
            "classificationId",
            "ClassificationId",
            "classificationID",
            "classification",
            "Classification",
        )
        week = id_from(hour_row, "week", "Week", "weekId", "WeekId", "weekNum", "WeekNum")
        hours = float(value_from(hour_row, "hours", "Hours", "numberHours", "NumberHours", default=0))
        hours_by_class_project_week[(classification_id, project_id, week)] = hours

    total_hours_by_class_project = {}
    for classification_id in classification_ids:
        for project_id in project_ids:
            total_hours_by_class_project[(classification_id, project_id)] = sum(
                hours_by_class_project_week.get((classification_id, project_id, week), 0)
                for week in weeks
            )

    return {
        "staff": staff,
        "projects": projects,
        "classifications": classifications,
        "staff_ids": staff_ids,
        "project_ids": project_ids,
        "classification_ids": classification_ids,
        "weeks": weeks,
        "staff_project_rate": staff_project_rate,
        "hours_by_class_project_week": hours_by_class_project_week,
        "total_hours_by_class_project": total_hours_by_class_project,
    }


def build_model(data):
    prob = pulp.LpProblem("Staff_Project_Assignment_Optimization", pulp.LpMinimize)
    assignment_vars = {}
    idle_hours_vars = {}

    for staff_id in data["staff_ids"]:
        for project_id in data["project_ids"]:
            assignment_vars[(staff_id, project_id)] = pulp.LpVariable(
                f"assign_staff_{staff_id}_project_{project_id}",
                lowBound=0,
                upBound=1,
                cat="Binary",
            )

    for staff_id in data["staff_ids"]:
        idle_hours_vars[staff_id] = pulp.LpVariable(
            f"idle_hours_staff_{staff_id}",
            lowBound=0,
            cat="Continuous",
        )

    objective = pulp.LpAffineExpression()
    for staff_id in data["staff_ids"]:
        staff_class = data["staff"][staff_id]["classification_id"]
        for project_id in data["project_ids"]:
            rate = data["staff_project_rate"].get((staff_id, project_id), 0)
            hours = data["total_hours_by_class_project"].get((staff_class, project_id), 0)
            objective += rate * hours * assignment_vars[(staff_id, project_id)]

    for staff_id in data["staff_ids"]:
        objective += 2 * data["staff"][staff_id]["base_rate"] * idle_hours_vars[staff_id]

    prob += objective, "total_cost"

    for project_id in data["project_ids"]:
        for classification_id in data["classification_ids"]:
            expr = pulp.LpAffineExpression()
            for staff_id in data["staff_ids"]:
                if data["staff"][staff_id]["classification_id"] == classification_id:
                    expr += assignment_vars[(staff_id, project_id)]
            prob += (expr == 1, f"project_{project_id}_classification_{classification_id}_requirement")

    for staff_id in data["staff_ids"]:
        staff_class = data["staff"][staff_id]["classification_id"]
        for week in data["weeks"]:
            expr = pulp.LpAffineExpression()
            for project_id in data["project_ids"]:
                hours = data["hours_by_class_project_week"].get((staff_class, project_id, week), 0)
                expr += hours * assignment_vars[(staff_id, project_id)]
            prob += (expr <= MAX_WEEKLY_HOURS, f"staff_{staff_id}_week_{week}_max_hours")

    for staff_id in data["staff_ids"]:
        staff_class = data["staff"][staff_id]["classification_id"]
        assigned_hours_expr = pulp.LpAffineExpression()
        for project_id in data["project_ids"]:
            hours = data["total_hours_by_class_project"].get((staff_class, project_id), 0)
            assigned_hours_expr += hours * assignment_vars[(staff_id, project_id)]
        prob += (
            idle_hours_vars[staff_id] == MAX_TOTAL_HOURS - assigned_hours_expr,
            f"staff_{staff_id}_idle_hours_definition",
        )

    return prob, assignment_vars, idle_hours_vars


def summarize_results(data, prob, assignment_vars, idle_hours_vars):
    staff_results = {}
    project_results = {}
    total_budgeted_staff_cost = 0
    total_idle_cost = 0

    for project_id in data["project_ids"]:
        project_results[project_id] = {
            **data["projects"][project_id],
            "assigned_staff": [],
            "total_cost": 0,
            "profit": data["projects"][project_id]["revenue"],
            "classification_week_hours": defaultdict(float),
        }

    for staff_id in data["staff_ids"]:
        staff_info = data["staff"][staff_id]
        staff_class = staff_info["classification_id"]
        assigned_projects = []
        budgeted_cost = 0
        billable_hours = 0
        weekly_project_hours = defaultdict(dict)

        for project_id in data["project_ids"]:
            if (assignment_vars[(staff_id, project_id)].varValue or 0) <= 0.5:
                continue

            hours = data["total_hours_by_class_project"].get((staff_class, project_id), 0)
            rate = data["staff_project_rate"].get((staff_id, project_id), 0)
            cost = hours * rate

            assigned_projects.append(project_id)
            budgeted_cost += cost
            billable_hours += hours
            project_results[project_id]["total_cost"] += cost
            project_results[project_id]["assigned_staff"].append(
                {
                    "staff_id": staff_id,
                    "name": staff_info["name"],
                    "classification": staff_info["classification"],
                    "hours": hours,
                    "cost": cost,
                }
            )

            for week in data["weeks"]:
                week_hours = data["hours_by_class_project_week"].get((staff_class, project_id, week), 0)
                weekly_project_hours[project_id][week] = week_hours
                project_results[project_id]["classification_week_hours"][(staff_info["classification"], week)] += week_hours

        idle_hours = idle_hours_vars[staff_id].varValue or 0
        idle_cost = 2 * staff_info["base_rate"] * idle_hours
        total_budgeted_staff_cost += budgeted_cost
        total_idle_cost += idle_cost

        staff_results[staff_id] = {
            **staff_info,
            "assigned_projects": assigned_projects,
            "budgeted_cost": budgeted_cost,
            "billable_hours": billable_hours,
            "non_billable_hours": idle_hours,
            "idle_cost": idle_cost,
            "weekly_project_hours": weekly_project_hours,
        }

    for project_id, project in project_results.items():
        project["profit"] = project["revenue"] - project["total_cost"]

    return {
        **data,
        "status": pulp.LpStatus[prob.status],
        "objective_value": pulp.value(prob.objective),
        "staff_results": staff_results,
        "project_results": project_results,
        "total_budgeted_staff_cost": total_budgeted_staff_cost,
        "total_idle_cost": total_idle_cost,
        "total_cost": total_budgeted_staff_cost + total_idle_cost,
    }


def run_optimization():
    data = prepare_data()
    prob, assignment_vars, idle_hours_vars = build_model(data)
    prob.solve(pulp.PULP_CBC_CMD(msg=False))
    return summarize_results(data, prob, assignment_vars, idle_hours_vars)


class StaffingOptimizationWindow(QMainWindow):
    def __init__(self):
        super().__init__()
        uic.loadUi(UI_FILE, self)

        self.data = None
        self.results = None
        self.figures = {}
        self.canvases = {}

        self.setup_chart("billable_pie", self.wPieBillable)
        self.setup_chart("billable_bar", self.wBarBillable)
        self.setup_chart("weekly_project", self.wHours)
        self.setup_chart("project_classification", self.wHoursClassification)

        self.btnRunOptimization.clicked.connect(self.run_optimization_clicked)
        self.cbEmployee.currentIndexChanged.connect(self.display_selected_employee)
        self.cbProject.currentIndexChanged.connect(self.display_selected_project)

        self.set_initial_state()
        self.load_dropdowns()

    def setup_chart(self, key, widget):
        layout = widget.layout()
        if layout is None:
            layout = QVBoxLayout(widget)
            layout.setContentsMargins(0, 0, 0, 0)

        figure = Figure(figsize=(3, 2))
        canvas = FigureCanvas(figure)
        layout.addWidget(canvas)
        self.figures[key] = figure
        self.canvases[key] = canvas

    def set_initial_state(self):
        self.lblStatus.setText("Not Run")
        self.lblEmployeeName.setText("...")
        self.lblClassifcation.setText("...")
        self.lblBaseRate.setText("...")
        self.lblProjectName.setText("...")
        self.lblIndustry.setText("...")
        self.lblRevenue.setText("...")
        self.lblTotalCosts.setText("...")
        self.label_8.setText("...")
        self.lblProfit.setText("...")
        self.wAssignedProjects.clear()
        self.wAssignedStaff.clear()
        for key in self.figures:
            self.draw_empty_chart(key, "Run optimization")

    def load_dropdowns(self):
        try:
            self.lblStatus.setText("Loading Data")
            QApplication.processEvents()
            self.data = prepare_data()
        except Exception as error:
            self.lblStatus.setText("Data Error")
            print(f"Data load error: {error}")
            return

        self.cbEmployee.blockSignals(True)
        self.cbEmployee.clear()
        for staff_id in self.data["staff_ids"]:
            staff = self.data["staff"][staff_id]
            self.cbEmployee.addItem(f"{staff_id} - {staff['name']}", staff_id)
        self.cbEmployee.blockSignals(False)

        self.cbProject.blockSignals(True)
        self.cbProject.clear()
        for project_id in self.data["project_ids"]:
            project = self.data["projects"][project_id]
            self.cbProject.addItem(f"{project_id} - {project['name']}", project_id)
        self.cbProject.blockSignals(False)

        self.lblStatus.setText("Ready")
        self.display_selected_employee()
        self.display_selected_project()

    def run_optimization_clicked(self):
        self.btnRunOptimization.setEnabled(False)
        self.lblStatus.setText("Optimization Running")
        QApplication.processEvents()

        try:
            self.results = run_optimization()
            self.data = self.results
        except Exception as error:
            self.lblStatus.setText("Optimization Error")
            print(f"Optimization error: {error}")
            self.btnRunOptimization.setEnabled(True)
            return

        self.lblStatus.setText("Optimization Complete")
        self.btnRunOptimization.setEnabled(True)
        self.display_selected_employee()
        self.display_selected_project()

    def display_selected_employee(self):
        if not self.data:
            return

        staff_id = self.cbEmployee.currentData()
        if staff_id is None:
            return

        staff = self.data["staff"][staff_id]
        self.lblEmployeeName.setText(staff["name"])
        self.lblClassifcation.setText(staff["classification"])
        self.lblBaseRate.setText(money(staff["base_rate"]))
        self.wAssignedProjects.clear()

        if not self.results:
            self.wAssignedProjects.addItem("Run optimization to populate")
            self.draw_empty_chart("billable_pie", "Run optimization")
            self.draw_empty_chart("billable_bar", "Run optimization")
            self.draw_empty_chart("weekly_project", "Run optimization")
            return

        staff_result = self.results["staff_results"][staff_id]
        for project_id in staff_result["assigned_projects"]:
            project = self.data["projects"][project_id]
            self.wAssignedProjects.addItem(f"{project_id} - {project['name']}")

        self.draw_billable_pie(staff_result)
        self.draw_billable_bar(staff_result)
        self.draw_weekly_project_stacked_bar(staff_result)

    def display_selected_project(self):
        if not self.data:
            return

        project_id = self.cbProject.currentData()
        if project_id is None:
            return

        project = self.data["projects"][project_id]
        self.lblProjectName.setText(project["name"])
        self.lblIndustry.setText(project["industry"])
        self.lblRevenue.setText(money(project["revenue"]))
        self.wAssignedStaff.clear()

        if not self.results:
            self.lblTotalCosts.setText("Run optimization")
            self.label_8.setText("Run optimization")
            self.lblProfit.setText("Run optimization")
            self.wAssignedStaff.addItem("Run optimization to populate")
            self.draw_empty_chart("project_classification", "Run optimization")
            return

        project_result = self.results["project_results"][project_id]
        self.lblTotalCosts.setText(money(project_result["total_cost"]))
        self.label_8.setText(money(self.results["total_cost"]))
        self.lblProfit.setText(money(project_result["profit"]))

        for staff in project_result["assigned_staff"]:
            self.wAssignedStaff.addItem(
                f"{staff['name']} - {staff['classification']} - {money(staff['cost'])}"
            )

        self.draw_project_classification_stacked_bar(project_result)

    def draw_empty_chart(self, key, message):
        figure = self.figures[key]
        canvas = self.canvases[key]
        figure.clear()
        ax = figure.add_subplot(111)
        ax.axis("off")
        ax.text(0.5, 0.5, message, ha="center", va="center", fontsize=9)
        figure.tight_layout()
        canvas.draw()

    def draw_billable_pie(self, staff_result):
        figure = self.figures["billable_pie"]
        canvas = self.canvases["billable_pie"]
        figure.clear()
        ax = figure.add_subplot(111)
        ax.pie(
            [staff_result["billable_hours"], staff_result["non_billable_hours"]],
            labels=["Billable", "Non-Billable"],
            autopct="%1.1f%%",
            startangle=90,
            colors=["#2f80ed", "#eb5757"],
        )
        ax.set_title("Billable vs Non-Billable")
        figure.tight_layout()
        canvas.draw()

    def draw_billable_bar(self, staff_result):
        figure = self.figures["billable_bar"]
        canvas = self.canvases["billable_bar"]
        figure.clear()
        ax = figure.add_subplot(111)
        hours = [staff_result["billable_hours"], staff_result["non_billable_hours"]]
        bars = ax.bar(
            ["Billable", "Non-Billable"],
            hours,
            color=["#2f80ed", "#eb5757"],
        )
        for bar, hour in zip(bars, hours):
            ax.text(
                bar.get_x() + bar.get_width() / 2,
                hour / 2,
                f"{hour:g}",
                ha="center",
                va="center",
                fontsize=12,
                color="black",
            )
        ax.set_title("Billable Location")
        ax.set_ylabel("Hours")
        figure.tight_layout()
        canvas.draw()

    def draw_weekly_project_stacked_bar(self, staff_result):
        figure = self.figures["weekly_project"]
        canvas = self.canvases["weekly_project"]
        figure.clear()
        ax = figure.add_subplot(111)

        weeks = self.data["weeks"]
        bottoms = [0] * len(weeks)
        colors = ["#2f80ed", "#27ae60", "#f2994a", "#9b51e0", "#56ccf2", "#f2c94c"]

        for index, project_id in enumerate(staff_result["assigned_projects"]):
            project = self.data["projects"][project_id]
            hours = [
                staff_result["weekly_project_hours"].get(project_id, {}).get(week, 0)
                for week in weeks
            ]
            ax.bar(
                [str(week) for week in weeks],
                hours,
                bottom=bottoms,
                label=project["name"],
                color=colors[index % len(colors)],
            )
            bottoms = [bottom + hour for bottom, hour in zip(bottoms, hours)]

        ax.set_title("Weekly Hours Across Projects")
        ax.set_xlabel("Week")
        ax.set_ylabel("Hours")
        ax.tick_params(axis="x", labelrotation=45)
        if staff_result["assigned_projects"]:
            ax.legend(fontsize=6, loc="upper right")
        figure.tight_layout()
        canvas.draw()

    def draw_project_classification_stacked_bar(self, project_result):
        figure = self.figures["project_classification"]
        canvas = self.canvases["project_classification"]
        figure.clear()
        ax = figure.add_subplot(111)

        weeks = self.data["weeks"]
        bottoms = [0] * len(weeks)
        colors = ["#2f80ed", "#27ae60", "#f2994a", "#9b51e0", "#56ccf2"]

        for index, classification in enumerate(self.data["classifications"].values()):
            hours = [
                project_result["classification_week_hours"].get((classification, week), 0)
                for week in weeks
            ]
            bars = ax.bar(
                [str(week) for week in weeks],
                hours,
                bottom=bottoms,
                label=classification,
                color=colors[index % len(colors)],
            )
            for bar, hour, bottom in zip(bars, hours, bottoms):
                if hour <= 0:
                    continue
                ax.text(
                    bar.get_x() + bar.get_width() / 2,
                    bottom + hour / 2,
                    f"{hour:g}",
                    ha="center",
                    va="center",
                    fontsize=9,
                    color="black",
                )
            bottoms = [bottom + hour for bottom, hour in zip(bottoms, hours)]

        ax.set_title("Hours by Classification")
        ax.set_xlabel("Week")
        ax.set_ylabel("Hours")
        ax.tick_params(axis="x", labelrotation=45)
        ax.legend(fontsize=8, loc="upper right", framealpha=0.9)
        figure.tight_layout()
        canvas.draw()


def print_report(results):
    for staff_id in results["staff_ids"]:
        staff = results["staff_results"][staff_id]
        assigned = ", ".join(str(project_id) for project_id in staff["assigned_projects"])
        print(f"{staff['name']}:")
        print(f"ID: {staff_id}")
        print(f"Projects Assigned: {assigned}")
        print(f"Budgeted Cost: {money(staff['budgeted_cost'])}")
        print(f"Idle Cost: {money(staff['idle_cost'])}")
        print()

    print(f"Total Budgeted Staff Cost: {money(results['total_budgeted_staff_cost'])}")
    print(f"Total Idle Cost: {money(results['total_idle_cost'])}")
    print(f"Total Cost: {money(results['total_cost'])}")


def main():
    if "--report" in sys.argv:
        print_report(run_optimization())
        return

    app = QApplication(sys.argv)
    window = StaffingOptimizationWindow()
    window.show()
    sys.exit(app.exec())


if __name__ == "__main__":
    main()

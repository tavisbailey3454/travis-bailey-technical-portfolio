# Comic Store Systems Analysis

Systems analysis project for an online comic store workflow, including as-is requirements and to-be improvements.

## What It Demonstrates

- Requirements analysis
- Use-case modeling
- Swimlane process mapping
- Class diagramming
- CRUDE matrix documentation
- CRC cards and method contracts
- Sequence, state machine, and package diagramming
- Connecting business processes to system responsibilities

## Functional Requirements Covered

- Manage comic inventory: add new comics, update existing comics, and remove comics from the system database.
- Browse and shop comics: display title, issue number, variant, publisher, published date, genre, and quantity.
- Manage a shopping cart: add in-stock comics, continue browsing, or proceed to checkout.
- Handle out-of-stock items: notify the customer, query for an in-stock alternative from the same genre, and add the alternative when accepted.
- Capture customer information and finalize an order with a unique order ID.

## To-Be Improvements

- Customer accounts and profiles: added a Customer Account class for name, email, password, profile creation, profile updates, and deletion.
- Editable shopping carts: added methods to change comic quantities or remove cart entries during the purchase process.

## Core Classes

- `Comic`: maintains title, issue number, variant ID, publisher, published date, genre, and quantity.
- `ShoppingCart`: stores selected comics and provides cart contents for order creation.
- `Order`: captures customer contact information, creates a unique order ID, and records purchased comics.
- `Customer Account`: supports returning customers by saving profile information and enabling member-specific flows.

## Included Artifact

- `ComicStore-systems-analysis.docx` - full systems analysis document with requirements, diagrams, CRC cards, CRUDE matrix, and method contracts.

## Resume-Ready Summary

Modeled an online comic store system using functional requirements, UML-style diagrams, CRC cards, a CRUDE matrix, and method contracts to document inventory management, customer accounts, editable carts, out-of-stock recommendations, checkout, and order creation.

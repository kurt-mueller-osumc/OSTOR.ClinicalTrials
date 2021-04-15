# OSTOR Clinical Trials ETL

## Rationale

This code attempts to adhere to the philosophy of "onion-level" architecture, where pure business logic, sans any inputs from csvs, xmls, https, sits inside a domain layer, accessible only through parsing and validation logic that wraps around it. Data from the outside world has to pass through these layers - each layer "vets" the data coming in and then either allows it to continue into the domain or shuttles it off into errors to be examined by the developer.

### Inputs

<!-- Values read from xmls and jsons get funneled into input variables, and then get validated before  -->
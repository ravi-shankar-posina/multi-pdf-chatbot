The provided ABAP code includes several forms that validate truck IDs and hub IDs against specific database tables. Here's a brief overview of each form:

1. **FORM validate_truckid**:
- This form validates the truck ID against the `ztmm_trkid` table.
- It checks if the global structure `gs_trkid` is not empty.
- If the truck ID is not found in the database, it sets the cursor to the field 'GS_TRKID-LOW' and displays an error message.

2. **FORM validate_dephub**:
- This form validates the departure hub against the `ztmm_hub` table.
- It checks if the global structure `gs_rehub` is not empty.
- If the departure hub ID is not found, it sets the cursor to the field 'GS_HUBID' and displays an error message.

3. **FORM validate_rechub**:
- This form validates the receiving hub against the `ztmm_hub` table.
- It checks if the global structure `gs_rehub` is not empty.
- If the receiving hub ID is not found, it sets the cursor to the field 'GS_REhub' and displays an error message.

4. **FORM f_get_truck_list**:
- This form is intended to retrieve transport details based on input provided on the selection screen.
- It refreshes the internal tables `gt_ztrkid_ds` and `gt_output_trk_ds` and clears the global structures `gi_trkid_ds` and `gi_truck_ds`.
- The code snippet ends before the SELECT statement is fully shown, but it appears to be preparing to select various fields related to truck details.

If you have specific questions about any part of the code or need further details, feel free to ask!
The provided ABAP code snippet is designed to retrieve and process data from a database table `ztmm_trkid` based on various selection criteria. Here's a breakdown of the key components:

1. **Data Retrieval**:
- The code selects multiple fields (`zuser`, `zreftrk`, `zreftrid`, `zfrwrder`, `zstatusid`, `zfullyrec`) from the `ztmm_trkid` table into an internal table `gt_ztrkid_ds`.
- The selection is filtered based on several conditions using the `IN` operator with various global variables (e.g., `gs_trkid`, `gs_hubid`, etc.).

2. **Sorting**:
- If the data retrieval is successful (checked using `sy-subrc`), the internal table `gt_ztrkid_ds` is sorted by `truckid`.

3. **Processing Logic**:
- The code contains logic to handle different scenarios based on the state of checkboxes (`gp_rec`, `gp_inr`, `gp_int`, `gp_cr`).
- If no checkboxes are selected, it loops through `gt_ztrkid_ds`, populating another structure `gi_truck_ds` with values from `gi_trkid_ds` and setting the status based on predefined constants (`gc_status11`, `gc_status9`, `gc_status8`).

4. **Conditional Processing**:
- If the "Received" checkbox (`gp_rec`) is selected, it loops through `gt_ztrkid_ds` again but only processes entries where the status is 'K' (Received) and `zfullyrec` is 'Y'.

5. **Appending Results**:
- The processed entries are appended to the output table `gt_output_trk_ds`.

This code is structured to handle different user selections and dynamically adjust the data processing accordingly. If you have specific questions about any part of the code or need further clarification, feel free to ask!
The provided ABAP code snippet processes data based on the status of trucks and appends relevant information to an output table (`gt_output_trk_ds`). It checks for three conditions based on checkboxes (`gp_inr`, `gp_int`, and `gp_cr`) to determine which records to process from the input table (`gt_ztrkid_ds`).

Here's a breakdown of the logic:

1. **In Receiving Checkbox (`gp_inr = 'X'`)**:
- Loops through `gt_ztrkid_ds` where the status is 'K' and `zfullyrec` is 'N'.
- Copies relevant fields from `gi_trkid_ds` to `gi_truck_ds`.
- Sets the status to `gc_status11` and appends `gi_truck_ds` to `gt_output_trk_ds`.

2. **In Transit Checkbox (`gp_int = 'X'`)**:
- Loops through `gt_ztrkid_ds` where the status is 'I'.
- Similar to the previous block, it copies fields and sets the status to `gc_status9`.

3. **Created Checkbox (`gp_cr = 'X'`)**:
- Loops through `gt_ztrkid_ds` where the status is 'H'.
- Again, it copies fields and sets the status to `gc_status8`.

After processing the records based on the selected checkboxes, the output table is sorted by `truckid`, and two subroutines (`build_fieldcatalog` and `display_output`) are called to handle the output display.

If you have specific questions about this code or need further clarification, feel free to ask!
The provided ABAP code consists of several forms that are part of a program designed to display a list of transport trucks using the ALV (ABAP List Viewer) functionality. Here's a breakdown of the key components:

1. **Form `build_fieldcatalog`**:
- This form is responsible for creating a field catalog for the ALV display.
- It refreshes the global table `gt_fldcat_ds` and populates it with field definitions using the `popu_fcatf` form.
- Each field is defined with its position, name, display text, and output length.

2. **Form `popu_fcatf`**:
- This form is called by `build_fieldcatalog` to populate the field catalog.
- It takes parameters for column position, table name, field name, display text, and output length.
- It creates a structure of type `slis_fieldcat_alv`, fills it with the provided parameters, and appends it to the global field catalog table `gt_fldcat_ds`.

3. **Form `display_output`**:
- This form calls the function module `REUSE_ALV_LIST_DISPLAY` to display the output in an ALV list format.
- It passes the field catalog and the output data table `gt_output_trk_ds` to the function module for display.

4. **Form `user_command`**:
- This form is intended to handle user commands, such as clicking on a TruckID to display more details about that specific truck.
- The implementation details for this form are not provided in the snippet.

Overall, the code is structured to create a user-friendly display of truck transport data, allowing for interaction through the ALV interface.
The provided ABAP code snippet is part of a program that processes data related to trucks and their associated collies (packages). Here's a breakdown of the key components:

1. **Data Declarations**:
- `lr_index`: A variable to hold the index of the selected item in a list.
- `lw_status`: A character field of length 15, likely used to store status messages.

2. **Refreshing Data**:
- `REFRESH gt_xcolli_ds`: Clears the internal table `gt_xcolli_ds`.

3. **Index Handling**:
- The code checks if `lr_index` is zero and increments it by one if true, ensuring it points to a valid entry.

4. **Reading Truck Data**:
- The program reads the truck data from the internal table `gt_output_trk_ds` using the index stored in `lr_index`.

5. **Selecting Header Data**:
- If the truck data is successfully read, it selects various fields from the `ztmm_trkid` table into the `gt_truck2_ds` internal table based on the truck ID.

6. **Getting Collies Details**:
- If the previous selection is successful, it retrieves collie details from the `ztmm_trkcol` table for all entries in `gt_truck2_ds`.

7. **Handling Unit Details**:
- If collie details are successfully retrieved, it selects additional details from the `ztmm_colli` table based on collie ID and other criteria.

8. **Error Handling**:
- The use of `sy-subrc` checks after each database operation ensures that the program only proceeds if the previous operation was successful.

This code is structured to ensure that data is retrieved in a logical sequence, with checks in place to handle potential errors at each step. If you have specific questions about this code or need further details, feel free to ask!
The provided ABAP code snippet is performing a series of database selections and data manipulations related to handling units and their associated details. Here's a breakdown of the key components:

1. **Data Selection from `vekp` Table**:
- The code selects various fields (`vpobjkey`, `vhilm_ku`, `vbeln_gen`) from the `vekp` table into the internal table `gt_vekp_ds` for all entries in `gt_colli_ds` where the condition `venum = gt_colli_ds-collint` is met.

2. **Conditional Checks**:
- After the selection, it checks if the selection was successful (`sy-subrc IS INITIAL`).
- If successful, it performs another selection from the `likp` table to get `vbeln` and `lifex` into `gt_likp_ds`, again based on entries in `gt_vekp_ds`.

3. **Sorting**:
- The internal table `gt_likp_ds` is sorted by `vbeln` if the previous selection was successful.

4. **Filtering Outbound Data**:
- The code creates a copy of `gt_vekp_ds` into `gt_vekp2_ds` and deletes entries where `vpobj` is not equal to '04', indicating that only outbound data is retained.

5. **Further Selection from `vttk` Table**:
- It selects `tknum` from the `vttk` table into `gt_vttk_ds` for all entries in `gt_vekp2_ds`, again checking for successful selection and sorting the results.

6. **Looping Through Handling Unit Details**:
- The code then loops through `gt_colli_ds`, populating the `gi_xcolli_ds` structure with various fields from `gi_colli_ds` and related data from `gt_vekp_ds`, `gt_likp_ds`, and `gt_trkcoli_ds` using `READ TABLE` statements.

7. **Status Description Retrieval**:
- The comment at the end suggests that the code is intended to retrieve status descriptions for certain status IDs, although the actual implementation for this part is not provided in the snippet.

Overall, the code is structured to gather and process data related to handling units, ensuring that only relevant outbound entries are considered, and it prepares the data for further processing or display.
The provided ABAP code snippet appears to be part of a larger program that processes transport-related data. Here's a breakdown of the key components:

1. **Status Handling**: The `CASE` statement is used to assign a specific status to `lw_status` based on the value of `gi_trkcoli_ds-status`. Each status ('A' to 'K') corresponds to a different constant (e.g., `gc_status1`, `gc_status2`, etc.).

2. **Reading from Internal Table**: The code reads an entry from the internal table `gt_vttk_ds` using the key `tknum` that matches `gi_vekp_ds-vpobjkey`. If a match is found (`sy-subrc IS INITIAL`), it assigns the `tknum` from `gi_vttk_ds` to `gi_xcolli_ds-tknum`.

3. **Appending Data**: After processing, `gi_xcolli_ds` is appended to the internal table `gt_xcolli_ds`, and several variables are cleared for the next iteration.

4. **Building Field Catalog**: The `PERFORM build_fieldcatalog2` is called to prepare a field catalog for displaying data, followed by `PERFORM display_output2` to display the output.

5. **Field Catalog Creation**: In the `build_fieldcatalog2` form, the field catalog `gt_fldcat2_ds` is refreshed, and constants are defined for the table name. The `popu_fcatf2` subroutine is called to populate the field catalog with specific fields and their properties.

This code is structured to handle transport data, manage statuses, and prepare for output display in a structured format. If you have specific questions about any part of the code or its functionality, feel free to ask!
The provided ABAP code snippets include several forms that are part of a larger program, likely for displaying data in an ALV (ABAP List Viewer) format. Here’s a brief overview of the key components:

1. **CURRENT_PAGE_RAW_OCR_TEXT**: This section appears to define a field catalog for an ALV report. Each entry specifies a column's position, table name, field name, and display attributes such as text and length.

2. **FORM popu_fcatf2**: This form populates a field catalog structure (`li_fld2_ds`) with parameters passed to it, such as column position, table name, field name, display text, and output length. It then appends this structure to a global field catalog table (`gt_fldcat2_ds`).

3. **FORM display_output2**: This form is responsible for displaying the output using the `REUSE_ALV_LIST_DISPLAY` function module. It sets up event handling, passes the field catalog, and handles any errors that may occur during the display process.

4. **FORM top_of_page**: This form is intended to manage the header display for the output screen, although the implementation details are not provided in the snippet.

If you have specific questions about any part of the code or need further clarification, feel free to ask!
The provided ABAP code consists of two main forms: `DISPLAY_SIDE_HEADING` and `DISPLAY_TRUCK_HEADER`.

### Key Components:

1. **Form `DISPLAY_SIDE_HEADING`:**
- This form is designed to display a side heading and its corresponding values in a formatted manner.
- It takes three pairs of headings and values as input parameters.
- The output is formatted with colors and intensification for better visibility.
- It uses the `WRITE` statement to output the headings and values at specified positions on the screen.

2. **Form `DISPLAY_TRUCK_HEADER`:**
- This form is responsible for displaying details of a selected truck.
- It initializes several local variables and reads data from an internal table `gt_truck2_ds`.
- It checks if the truck is fully unloaded or fully loaded based on the values in the `gi_truck2_ds` structure.
- The form includes a `SELECT` statement to retrieve additional data related to the truck.

### Summary of Functionality:
- The `DISPLAY_SIDE_HEADING` form is used for structured output of headings and values, while the `DISPLAY_TRUCK_HEADER` form focuses on displaying truck-related information, checking conditions, and preparing data for display.

If you have specific questions about the code or need further details, feel free to ask!
The provided ABAP code snippet is part of a program that processes data related to truck collections and goods receipts. Here's a breakdown of the key components and logic:

1. **Data Selection**:
- The code first selects data from the `ztmm_trkcol` table into the internal table `gt_trkcoli_ds` for all entries in `gt_truck2_ds` where the truck ID matches and the status is 'I' (indicating some form of incomplete status) and `zrelgr` is 'Y'.
- If no records are found (`sy-subrc IS INITIAL`), it sets a flag `gi_truck2_ds-fullgr` to 'N'.

2. **Checking for Received Items**:
- A second selection checks if all items in `ztmm_trkcol` have been received by querying again with the same conditions but without the status check.
- If records are found, it proceeds to select data from the `ztmm_colli` table based on the collected IDs and external IDs.

3. **Inbound Delivery Processing**:
- For each relevant colli, it retrieves the inbound delivery number from the `vekp` table.
- It checks if the inbound delivery has been goods-received by querying the `vbfa` table for records where the delivery number matches and the document type indicates a goods movement (specifically types 'R' for goods receipt).

4. **Conditional Logic**:
- The code uses nested IF statements to ensure that each step is contingent on the successful retrieval of data from the previous step, indicating a structured approach to data validation and processing.

This code is typical in logistics and supply chain management systems where tracking the status of deliveries and goods receipts is crucial for operational efficiency.
The provided ABAP code snippet appears to be part of a larger program that processes truck data, specifically checking the status of collies (packages) and updating their status in a database table. Here’s a breakdown of the key components:

1. **Checking if all collies have been GR (Goods Receipt)**:
- The code checks if the number of lines in the `gt_vbfa_ds` table matches the number of lines in the `lw_vekp_lines` variable.
- If they match, it sets the `fullgr` field of `gi_truck2_ds` to 'Y' and updates the corresponding record in the `ztmm_trkid` table to indicate that all collies have been received.
- If they do not match, it sets `fullgr` to 'N'.

2. **Getting the status description**:
- A `CASE` statement is used to assign a status description to `lw_status` based on the value of `gi_truck2_ds-status`. Each status code (from 'A' to 'K') corresponds to a specific description stored in constants (e.g., `gc_status1`, `gc_status2`, etc.).

3. **Displaying information**:
- The `PERFORM` statements are used to call a subroutine (`display_side_heading`) to display the truck ID and status, along with a plate number.

This code is structured to ensure that the status of trucks and their associated collies is accurately tracked and updated in the system. If you have specific questions about any part of the code or need further clarification, feel free to ask!
The provided ABAP code snippet appears to be part of a report or a module that displays information related to transportation, specifically focusing on truck shipments. Here's a breakdown of the key components:

1. **Display of Truck Information**:
- The code uses the `PERFORM` statement to call a subroutine named `display_side_heading`, which is likely responsible for displaying headings and associated data for various fields related to the truck shipment.
- Fields such as "From Hub", "To Hub", "Forwarder", "Departure date", "Departure time", and others are displayed using the `WRITE` statement.

2. **Transport Type Handling**:
- A `CASE` statement is used to determine the transport type based on the value of `gi_truck2_ds-trantype`. Each transport type (e.g., Truck, Train, Container, etc.) is associated with a specific constant (e.g., `gc_tran_type1`, `gc_tran_type2`, etc.) that presumably holds a description or identifier for that transport type.

3. **Formatting and Output**:
- The output is formatted with colors and styles using the `FORMAT` statement, which enhances the visual presentation of the data.
- The `WRITE` statement is used to output various pieces of information, including comments and loading meters.

4. **User Command Handling**:
- The `FORM user_command1` subroutine is defined to handle user commands, specifically when a user clicks on "Handling Unit Details". This suggests that the program is interactive and allows users to navigate through different screens or details related to shipments.

5. **Data Types**:
- The code uses various data types, such as `vbeln_nach` for `lw_gr_mat_doc`, which indicates that it is likely dealing with document numbers or identifiers related to shipments.

Overall, this code snippet is part of a larger ABAP program that manages and displays transportation-related data, focusing on truck shipments and their details. If you have specific questions about certain parts of the code or need further clarification, feel free to ask!
The provided ABAP code snippet is part of a program that retrieves and processes delivery and material document information based on a specific "Collint" (colli number). Here's a breakdown of the key components and logic:

1. **Variable Declarations**:
- `lw_year`, `lw_gr_createdby`, and `lw_status` are declared to hold specific data types related to the creation date, user name, and status ID, respectively.
- `lr_index1` is used to store the index of a selected item from a list.

2. **Initialization**:
- The internal tables `gt_final_ds`, `gt_vekp_ds`, `gt_vbfa_ds`, and `gt_likp_ds` are refreshed to clear any previous data.

3. **Index Handling**:
- The index `lr_index1` is set based on the selected item from a list. If no item is selected (index is 0), it is incremented to 1.

4. **Data Retrieval**:
- The code checks if the `collint` field of the `gi_xcolli_ds` structure is not empty. If it has a value, it performs a series of SQL SELECT statements:
- It retrieves delivery details from the `vekp` table based on the `collint`.
- If deliveries are found, it fetches related delivery information from the `likp` table.
- It sorts the results by delivery number (`vbeln`).

5. **Material Document and User Information**:
- The code then retrieves material document details from the `vbfa` table, filtering for goods movements (type 'R') and specific movement types (101, 103).
- If material documents are found, it retrieves the corresponding entries from the `mkpf` table to get the creation username (`usnam`) and year of the document.

6. **Final Data Storage**:
- The relevant information (material document number, year, and creator username) is stored in the variables `lw_gr_mat_doc`, `lw_year`, and `lw_gr_createdby`.

This code is structured to ensure that it only processes data when valid entries are found, and it uses efficient SQL operations to gather necessary information from multiple related tables.
The provided ABAP code snippet is part of a larger program that processes data related to shipments and goods movements. Here's a breakdown of the key components:

1. **Data Retrieval**: The code uses multiple `SELECT` statements to retrieve data from various database tables (`vepo`, `vbuk`, `vbfa`, `mkpf`, `likp`) based on certain conditions.

2. **Conditional Logic**: The code checks the success of each `SELECT` operation using `sy-subrc`. If the value is `INITIAL`, it indicates that the previous operation was successful, and the code proceeds to the next steps.

3. **Table Operations**:
- The `READ TABLE` statement is used to read specific entries from internal tables (e.g., `gt_vekp_ds`, `gt_vbfa1_ds`, `gt_mkpf1_ds`, `gt_likp1_ds`).
- The `SORT` statement is used to sort the internal tables based on specific fields.

4. **Data Filtering**: The `FOR ALL ENTRIES IN` clause is used to filter records based on the entries in another internal table, which is a common practice in ABAP to optimize data retrieval.

5. **Comments**: The code includes comments (e.g., `"Shipment`, `" Goods Movement`) that provide context for certain operations.

Overall, this code is structured to handle complex data relationships in an SAP environment, specifically focusing on shipments and their associated documents. If you have specific questions about any part of the code or its functionality, feel free to ask!
The provided ABAP code snippet appears to be part of a larger program that processes delivery and shipment data. Here's a breakdown of the key components and logic:

1. **Data Selection**:
- The code retrieves data from the `lips` table (which contains item data for deliveries) into the internal table `gt_lips_ds` based on entries in `gt_vepo_ds` (which likely contains delivery header data).
- If the selection is successful (`sy-subrc IS INITIAL`), it sorts the `gt_lips_ds` table by `vbeln` (delivery number) and `posnr` (item number).

2. **Handling Inbound Deliveries**:
- If the object type (`vpobj`) is '03' or '12', it selects data from the `likp` table (which contains delivery header data) into `gt_likp1_ds` based on the delivery numbers in `gt_vekp_ds`.
- It then retrieves item data from `lips` into `gt_lips1_ds` for the selected deliveries and sorts it if the selection is successful.

3. **Processing Shipments**:
- If the object type is '04' (indicating a shipment), the code loops through `gt_lips_ds` to populate `gi_final_ds` with relevant data.
- It reads from `gt_likp1_ds` to get additional details like `vstel` (shipping point), `route`, and `werks` (plant).
- It also reads from `gt_vepo_ds` to get material number, quantity, and unit of measure.

4. **Data Structure**:
- The code uses various internal tables (`gt_lips_ds`, `gt_lips1_ds`, `gt_likp1_ds`, `gt_vepo_ds`) and structures (`gi_lips_ds`, `gi_likp1_ds`, `gi_vepo_ds`, `gi_final_ds`) to hold and manipulate data throughout the process.

5. **Error Handling**:
- The use of `sy-subrc` checks after each database operation ensures that the program only proceeds if the previous operation was successful.

This code is typical in SAP ABAP for handling logistics data, particularly in scenarios involving deliveries and shipments. If you have specific questions about parts of the code or need further clarification, feel free to ask!
The provided ABAP code snippet appears to be part of a larger program that processes delivery and handling unit data. Here’s a breakdown of the key components and logic:

1. **Data Retrieval**: The code reads data from various internal tables (`gt_vbuk_ds`, `gt_vekp_ds`, `gt_likp_ds`, etc.) based on specific keys. It checks if the read operation was successful using `sy-subrc`.

2. **Conditional Logic**:
- If certain conditions are met (e.g., `gi_vbuk_ds-wbstk = 'C'`), it performs additional reads and assigns values to `gi_final_ds` from other data structures.
- There is a specific handling for inbound deliveries or non-assigned handling units, indicated by the condition checking `gi_vekp_ds-vpobj`.

3. **Data Assignment**: The code assigns various fields from the source data structures to the `gi_final_ds` structure, which seems to be a final output structure that aggregates the relevant information.

4. **Appending Data**: After processing, the `gi_final_ds` structure is appended to the `gt_final_ds` internal table, which likely holds the final results of the processing.

5. **Clearing Variables**: The code clears the working variables after each loop iteration to prepare for the next set of data.

6. **Looping**: The code uses nested loops to iterate over the data, indicating that it processes multiple records in a structured manner.

This code is typical in ABAP for handling complex data processing tasks, especially in logistics and supply chain scenarios. If you have specific questions about parts of the code or need further clarification, feel free to ask!
The provided ABAP code snippet appears to be part of a larger program that processes data related to deliveries and builds a field catalog for displaying output in an ALV (ABAP List Viewer) format. Here’s a breakdown of the key components:

1. **Data Processing Loop**:
- The code reads data from a table (`gt_likp_ds`) based on a key (`vhilm_ku`) derived from another structure (`gi_vekp_ds`).
- If a matching entry is found (`sy-subrc IS INITIAL`), it assigns a value (`lifex`) to a field (`ext_del_no`) in the final data structure (`gi_final_ds`).
- The final data structure is then appended to a final output table (`gt_final_ds`), and temporary structures are cleared for the next iteration.

2. **Field Catalog Creation**:
- The `build_fieldcatalog3` form is responsible for creating a field catalog (`gt_fldcat3_ds`) for the ALV display.
- It defines a constant for the table name and calls another form (`popu_fcatf3`) multiple times to populate the field catalog with various fields, their display texts, and output lengths.

3. **Field Catalog Population**:
- The `popu_fcatf3` form takes parameters such as column position, table name, field name, display text, and output length to create a field catalog entry.
- Each entry is appended to the `gt_fldcat3_ds` internal table, which will be used for displaying the ALV output.

### Key Points:
- The code is structured to handle data processing and display in a modular way using forms.
- It utilizes internal tables and structures to manage data efficiently.
- The field catalog is crucial for defining how data will be presented in the ALV report.

If you have specific questions about certain parts of the code or need further clarification, feel free to ask!
The provided ABAP code snippets include several forms that handle different functionalities. Here's a brief overview of each form:

1. **Form `display_output3`**:
- This form is responsible for displaying output using the ALV (ABAP List Viewer) function module `REUSE_ALV_LIST_DISPLAY`.
- It sets parameters for the display, such as bypassing the buffer, saving options, and specifying the field catalog (`gt_fldcat3_ds`).
- The output table is `gt_final_ds`, which contains the data to be displayed.

2. **Form `f4_user`**:
- This form is designed to provide a value help (F4 help) for user selection.
- It defines a structure `ty_user` and a table `gt_user` to hold distinct user values fetched from the database table `ztmm_trkid`.
- The function module `F4IF_INT_TABLE_VALUE_REQUEST` is called to display the F4 help dialog, allowing users to select a value from the `gt_user` table.
- If values are found, the user list is sorted by the `zuser` field.

3. **Form `f4_retrk`**:
- Similar to `f4_user`, this form provides F4 help for reference tracking.
- It defines a structure `ty_reftrk` and a table `gt_reftrk` to hold distinct reference tracking values fetched from the same database table `ztmm_trkid`.
- The code snippet ends abruptly, so the complete functionality of this form is not visible.

Overall, these forms are part of a larger ABAP program that likely deals with displaying lists and providing user input options through F4 help dialogs. If you have specific questions about any part of the code or need further details, feel free to ask!
The provided ABAP code snippets define two forms, `F4_RETRK` and `F4_RTRID`, which are used to handle value selection for two different fields, `ZREFTRK` and `ZREFTRID`, respectively.

### Breakdown of the Code:

1. **Form F4_RETRK**:
- It defines a table `gt_reftrk` to hold distinct values of `ZREFTRK` from a database table.
- The `F4IF_INT_TABLE_VALUE_REQUEST` function module is called to display a value selection dialog for the field `GS_RETRK`.
- If values are found (checked by `sy-subrc`), the table `gt_reftrk` is sorted by `zreftrk`.

2. **Form F4_RTRID**:
- Similar to `F4_RETRK`, this form defines a table `gt_reftrid` to hold distinct values of `ZREFTRID`.
- It also calls the `F4IF_INT_TABLE_VALUE_REQUEST` function module for the field `GS_RTRID`.
- Again, if values are found, the table `gt_reftrid` is sorted by `zreftrid`.

### Key Points:
- Both forms utilize the same function module to provide a user-friendly selection interface for the respective fields.
- The use of `SELECT DISTINCT` ensures that only unique values are fetched from the database.
- The sorting of the internal tables after fetching values is a common practice to ensure that the values are presented in a sorted manner to the user.

If you have specific questions about the code or need further details, feel free to ask!

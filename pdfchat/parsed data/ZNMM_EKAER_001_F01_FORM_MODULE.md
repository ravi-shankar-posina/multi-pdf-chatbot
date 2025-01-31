The provided ABAP code snippet includes several form routines that are part of a larger program. Here's a brief overview of the key components:

1. **get_data**: This form retrieves data from the database and checks if any records were retrieved. If no records are found, it displays a message and stops the execution. It also calls other forms to fill buffers and prepare output.

2. **show_data**: This form is responsible for displaying data. It calls another form to show a specific piece of information and then calls a screen (screen '0001') to present the data to the user.

3. **set_field_catalog**: This form prepares a field catalog for a table. It calls the function module `LVC_FIELDCATALOG_MERGE` to merge the field catalog based on the structure 'ZSMM_EKAER_001'.

4. **get_type_line**: This form retrieves the type of a line based on the output data. It uses the class `cl_abap_tabledescr` to describe the table and sets the line type to 'ZSMM_EKAER_001'.

5. **show_package_pi**: This form is defined but not fully shown in the provided snippet. It likely deals with displaying or processing package information.

Overall, the code is structured to handle data retrieval, processing, and display in an ABAP program, with a focus on modularity through the use of form routines.
The provided ABAP code snippets consist of several form routines that handle different functionalities. Here's a brief overview of each form:

1. **show_package_pi**:
- This form increments a counter (`pd_count`), concatenates it with a message (`text-m02`), and then calls another form (`show_pi`) to display the message using a progress indicator.

2. **show_pi**:
- This form takes a message (`pd_msg`) as input and calls the SAP function module `SAPGUI_PROGRESS_INDICATOR` to display the message in the SAP GUI.

3. **set_layout**:
- This form sets the layout for a grid by calling another form to set the grid title and configuring layout options such as column width and selection mode.

4. **set_variant**:
- This form initializes a variant structure (`gs_variant`) with the current report ID (`sy-repid`) and a handle value.

5. **get_from_db**:
- This form retrieves data based on a case (`ld_case`). It uses a `CASE` statement to determine the type of data to fetch (Delivery, Shipment, or Trucking) and calls the appropriate forms to get the data.

6. **fill_buffers**:
- This form declares two data objects (`so_kunnr` and `so_matnr`) as ranges for customer numbers and material numbers, respectively. The implementation details are not provided in the snippet.

If you have specific questions about any part of the code or need further clarification, feel free to ask!
The provided ABAP code snippet appears to be part of a larger program that processes input data, specifically focusing on customer and material information. Here's a breakdown of the key components and their functionality:

1. **Data Declarations**:
- `ls_kunnr` and `ls_matnr` are declared to hold customer and material information, respectively, with the same structure as the corresponding tables `so_kunnr` and `so_matnr`.

2. **Field Symbols**:
- Field symbols `<fs_input>`, `<fs_kunnr>`, and `<fs_matnr>` are defined to dynamically reference lines in the internal tables `gt_input`, `so_kunnr`, and `so_matnr`.

3. **Input Check**:
- The code checks if there are any entries in `gt_input`. If there are none, the subsequent processing is skipped.

4. **Looping Through Input**:
- The code loops through each entry in `gt_input`:
- It attempts to read customer numbers (`kunnr_to` and `kunnr_ship`) from `so_kunnr`. If the customer number is not found, it adds it to `so_kunnr`.
- It also reads material numbers from `so_matnr` and adds them if they are not already present.

5. **Filling Buffers**:
- After processing the input, the program calls subroutines (`PERFORM`) to fill buffers with customer and material data:
- `get_kna1` is called to retrieve customer data based on `so_kunnr`.
- `get_mara_marc` is called to retrieve material data based on `so_matnr`.
- `fill_last_document` is called to handle previous document data.

6. **Exchange Rate Buffer**:
- A SELECT statement retrieves exchange rate data from the `tcurr` table, specifically for the currency 'HUF'.

7. **End of Form**:
- The form `fill_buffers` is concluded, indicating that this section of code is encapsulated within a subroutine.

### Summary
This ABAP code is designed to process input data related to customers and materials, ensuring that unique entries are maintained in the respective tables. It also prepares buffers for further processing, including customer and material details, and retrieves exchange rate information.
The provided ABAP code snippet appears to be part of a larger program that processes input data related to shipments and customer information. Here's a breakdown of the key components and functionality:

1. **Data Declarations**:
- `ls_output`: A structure that holds the output data, defined to be like a line of the internal table `gt_output`.
- `FIELD-SYMBOLS`: These are dynamic references to lines of various internal tables (`gt_vekp`, `gt_vbfa`, `gt_kna1`, `gt_marc`, `gt_mara`, `gt_input`), allowing for flexible data manipulation.

2. **Main Processing Logic**:
- The code begins by calling a subroutine `show_pi` with a parameter `text-m04`.
- It then loops through the `gt_input` table, processing each entry.

3. **Shipment Number Handling**:
- The code attempts to read a shipment number from the `gt_vbfa` table using the key fields `vbelv` and `vbtyp_n`.
- If a matching entry is found, it assigns the shipment number to `ls_output-tknum`.
- It checks if this shipment number is not in the selection table `s_tknum`. If not, it marks the input for deletion.

4. **Truck ID Validation**:
- A subroutine `get_truckid` is called to retrieve the truck ID based on the shipment number.
- If the truck ID is not found in `s_truk`, the input is again marked for deletion.

5. **Customer Data Handling**:
- The code checks if the `kunnr_to` (customer number to) is initial. If so, it assigns it the value of `kunnr_ship` (customer number ship).
- It then reads customer details from the `gt_kna1` table for both `kunnr_to` and `kunnr_ship`, populating various fields in `ls_output` with customer information such as name, address, and country.

6. **Conditional Checks**:
- The code uses `CHECK` statements to ensure that only valid entries are processed further, based on the deletion flags.

Overall, this code is designed to process shipment and customer data, ensuring that only valid records are retained and that necessary customer information is populated in the output structure.
The provided ABAP code snippets appear to be part of a larger program that processes shipping and material data. Here's a breakdown of the key components:

1. **Shipping Information Extraction**:
- The code retrieves shipping address details from the KNA1 table (customer master data) using the fields `ort01`, `stras`, and `regio`.
- It checks if the data was successfully read using `sy-subrc`.

2. **Material Information Retrieval**:
- The code reads material data from the MARC (plant data for material) and MARA (general material data) tables.
- It calculates the gross weight (`brgew`) based on the material's weight and the quantity (`lfimg`).

3. **Delivery Value Calculation**:
- The `get_delivery_value` subroutine is called to perform additional calculations related to delivery.
- The net price (`netpr`) and currency (`waers`) are assigned to the output structure.

4. **Output Preparation**:
- The net value (`netwr`) is calculated by multiplying the quantity, net price, and exchange rate (`ukurs`).
- The output structure (`ls_output`) is appended to an output table (`gt_output`).

5. **Deletion Logic**:
- If a flag (`ld_to_delete`) indicates that certain entries should be deleted, those entries are removed from the input table (`gt_input`).

6. **Shipment Processing**:
- The `get_shipments` subroutine processes shipment data based on a case (`pd_case`).
- It opens a cursor to select shipment-related data from the VBFA table (document flow).

7. **Conditional Logic**:
- The code uses conditional statements to handle different cases for shipment processing, such as 'D' for delivery and 'S' for another type of shipment.

This code is structured to handle data extraction, calculations, and conditional processing related to shipping and materials in an SAP environment. If you have specific questions about any part of the code or need further clarification, feel free to ask!
The provided ABAP code snippets are part of a program that handles the retrieval of shipment and delivery data from SAP tables. Here's a breakdown of the key components:

1. **Cursor Handling**:
- The code uses cursors (`gc_cursor` and `lc_cursor`) to fetch data from the database in a controlled manner, allowing for efficient processing of large datasets.

2. **Conditional Logic**:
- The `CASE` statement is used to differentiate between different processing scenarios based on the value of `pd_case`. For example, when `pd_case` is 'T', it fetches data from the `vbfa` table, while when it is 'D', it retrieves data from the `likp` and `lips` tables.

3. **Data Selection**:
- The `SELECT` statements specify the fields to be retrieved from the database tables. For instance, in the 'D' case, it selects various fields from the `likp` and `lips` tables, joining them on the `vbeln` field.

4. **Filtering Conditions**:
- The `WHERE` clause in the `SELECT` statements includes multiple conditions that filter the data based on the values in the selection ranges (e.g., `s_vbeln`, `s_erdat`, etc.).

5. **Fetching Data**:
- The `FETCH NEXT CURSOR` statement is used to retrieve the next set of records from the cursor into the internal table `gt_vbfa`, with a specified package size of 1000.

6. **Looping Through Results**:
- A `WHILE` loop is employed to continue fetching data until there are no more records to process (`sy-subrc` is not equal to 0).

7. **Subroutine Calls**:
- The `PERFORM` statement is used to call a subroutine (`show_package_pi`) that likely handles the display or processing of the data fetched.

8. **End of Processing**:
- The cursor is closed after all data has been processed to free up resources.

This code is structured to efficiently handle large volumes of data while ensuring that the necessary conditions for data retrieval are met. If you have specific questions about any part of the code or need further clarification, feel free to ask!
The provided ABAP code snippet is part of a larger program that handles the retrieval of delivery data from the database. Here's a breakdown of the key components:

1. **Cursor Declaration**: The code opens a cursor (`lc_cursor`) to fetch data from the database based on certain conditions.

2. **Data Selection**:
- The `SELECT` statement retrieves various fields from the `likp`, `lips`, and `vbuk` tables.
- The fields include delivery number (`vbeln`), item number (`posnr`), dates (`erdat`, `wadat_ist`), and other relevant delivery and item details.

3. **Join Conditions**:
- The `likp` table (header data) is joined with the `lips` table (item data) and the `vbuk` table (status data) based on the delivery number.

4. **Dynamic Selection**:
- The `FOR ALL ENTRIES IN` clause allows for dynamic selection based on the contents of either `gt_vbfa` or `gt_vekp`, depending on the case ('S' or 'T').

5. **Filtering Conditions**:
- The `WHERE` clause filters the results based on various selection criteria (e.g., delivery date, delivery status, customer number).

6. **Fetching Data**:
- After opening the cursor, the program fetches data in chunks of 1000 records using `FETCH NEXT CURSOR` and appends the results to the internal table `gt_input`.

7. **Looping Through Results**:
- A `WHILE` loop continues to fetch data until there are no more records to retrieve (`sy-subrc` is not equal to 0).

8. **Closing the Cursor**:
- Finally, the cursor is closed after all data has been fetched.

9. **Subroutine Call**:
- The `PERFORM show_package_pi USING ld_count` is called to presumably display or process the count of records fetched.

This code is structured to efficiently handle large datasets by using cursors and chunked fetching, which is a common practice in ABAP programming to manage memory and performance.
The provided ABAP code snippets contain three forms: `get_delivery_value`, `validate_input`, and `get_truckid`. Here’s a brief overview of each:

1. **get_delivery_value**:
- This form retrieves delivery values based on input parameters.
- It checks the `gt_vbap` table for a specific sales document and item number. If found, it assigns the net price and currency to the input structure.
- If not found, it looks up the `gt_ekpo` table using the purchase order number and item number.
- Finally, it calls another form `get_exchange_rate` to presumably handle currency conversion.

2. **validate_input**:
- This form checks if at least one of several input fields (`s_vstel`, `s_vkoiv`, `s_vbeln`, `s_tknum`, `s_truk`) is not initial (i.e., has a value).
- If all fields are initial, it raises an error message using a message class `zmm`.

3. **get_truckid**:
- This form retrieves a truck ID based on a given truck number.
- It loops through the `gt_vekp` table, which is assumed to contain vehicle data, and checks for a non-initial truck ID associated with the provided truck number.
- If found, it assigns the truck ID to the output parameter and exits the loop.

If you have specific questions about any part of the code or need further details, feel free to ask!
The provided ABAP code snippets consist of two forms: `set_grid_title` and `get_exchange_rate`. Here’s a brief overview of each:

### `set_grid_title`
- **Purpose**: This form sets a title for a grid based on the number of lines associated with unique sales document numbers (`vbeln`).
- **Key Components**:
- A hashed table `lt` is defined to store unique sales document numbers and their corresponding line counts.
- The loop iterates over an input table `gt_input`, checking if each `vbeln` exists in `lt`. If not, it initializes a new entry.
- The line count for each `vbeln` is incremented.
- Finally, it constructs a title string by concatenating a predefined text (`text-m06`) with the total line count.

### `get_exchange_rate`
- **Purpose**: This form retrieves the exchange rate for a given currency based on the current date.
- **Key Components**:
- It initializes a date variable `ld_erdat` to the previous day of the current date.
- It checks if the currency (`waers`) is not 'HUF' (Hungarian Forint) and sets a default exchange rate (`ukurs`) to 1.
- It converts the date format using the function module `CONVERSION_EXIT_INVDT_INPUT`.
- It loops through a table `gt_tcurr` to find the exchange rate for the specified currency and date.
- If a valid exchange rate is found, it updates `ps_input-ukurs` with the found rate.

### Summary
- The first form is focused on counting and displaying the number of unique sales documents, while the second form is concerned with fetching the exchange rate for a specified currency based on the current date. Both forms utilize field symbols for efficient data handling and manipulation.
The provided ABAP code snippets contain two forms: `get_kna1` and `get_mara_marc`.

### `get_kna1` Form
- **Purpose**: This form retrieves customer data from the `kna1` table based on a list of customer numbers (`pso_kunnr`).
- **Logic**:
- It initializes a counter (`ld_count`) and a field-symbol for customer numbers.
- It loops through the input customer numbers, appending them to a temporary table (`so_kunnr`).
- When the count exceeds a predefined constant (`co_package`), it performs a database select to fetch customer details from `kna1` and appends the results to `gt_kna1`.
- After the loop, if there are remaining customer numbers, it performs another select to fetch the remaining data.

### `get_mara_marc` Form
- **Purpose**: This form retrieves material data from the `marc` table based on a list of material numbers (`pso_matnr`).
- **Logic**:
- Similar to `get_kna1`, it initializes a counter (`ld_count`) and a field-symbol for material numbers.
- It loops through the input material numbers, appending them to a temporary table (`so_matnr`).
- When the count exceeds 1000, it performs a database select to fetch material details from `marc` and appends the results to `gt_marc`.

### Key Points
- Both forms utilize a loop to process input data in batches to avoid performance issues with large datasets.
- They use field-symbols for efficient data handling.
- The forms are designed to handle large input lists by breaking them into manageable chunks based on predefined limits.

If you have specific questions about the code or need further details, feel free to ask!
The provided ABAP code snippets include two main sections: one for retrieving material data from the MARA and MARC tables, and another for defining data structures and performing operations related to trucking and handling units.

### Key Components:

1. **Data Retrieval from MARA and MARC:**
- The code retrieves material numbers (`matnr`), gross weight (`brgew`), and net weight (`gewei`) from the `mara` table and stores them in an internal table `gt_mara`.
- It also retrieves plant-specific data (`werks`, `matnr`, `stawn`) from the `marc` table and appends it to `gt_marc`.
- The selection is based on a range of material numbers defined in `so_matnr`.

2. **Data Structures:**
- Two structured types are defined:
- `lty_truk`: Represents trucking information with fields like `ztrukid`, `zcolliid`, etc.
- `lty_hu`: Represents handling unit information with fields like `venum`, `vbeln`, etc.
- Internal tables `lt_truk` and `lt_hu` are declared to hold data of these types.

3. **Field Symbols:**
- Field symbols `<fs_truk>` and `<fs_hu>` are declared to reference lines of the internal tables `lt_truk` and `lt_hu`, respectively.

4. **Form Routine:**
- The form `get_trucking1` is defined to encapsulate the logic for handling trucking data, taking `pd_case` as an input parameter.

### Summary:
The code is structured to perform data retrieval from material-related tables in SAP, define necessary data structures for trucking and handling units, and encapsulate the logic in a form routine for better modularity. The use of `APPENDING TABLE` indicates that the results are being collected into internal tables for further processing.
The provided ABAP code snippet is structured to handle different cases based on the value of `pd_case`. Here's a breakdown of the logic for each case:

1. **Case 'D'**:
- It checks if there are any entries in `gt_input`.
- If there are entries, it performs a `SELECT` query to join tables `vekp` and `vepo` to retrieve specific fields into the internal table `lt_hu` based on the `vbeln` from `gt_input`.
- If `lt_hu` has entries, it performs another `SELECT` query to join `ztmm_trkcol` and `ztmm_colli` to retrieve additional fields into `lt_truk`, filtering by `zcollint` which matches `venum` from `lt_hu`.

2. **Case 'S'**:
- It directly performs a `SELECT` query to join `vekp` and `vepo` to fill `lt_hu` where `vpobj` equals '04' and `vpobjkey` is in the selection table `s_tknum`.
- If `lt_hu` has entries, it performs a similar `SELECT` query as in case 'D' to fill `lt_truk`.

3. **Case 'T'**:
- It performs a `SELECT` query to retrieve data from `ztmm_trkcol` and `ztmm_colli` into `lt_truk` where `ztrukid` is in the selection table `s_truk`.
- The code snippet ends abruptly, indicating that there may be additional logic that follows.

### Summary:
- The code is designed to handle three different scenarios based on the value of `pd_case`, performing database queries to populate internal tables based on specific conditions and relationships between the tables involved. Each case has its own logic for selecting data from the database.
The provided ABAP code snippets include several forms that handle data selection and processing related to shipments and documents. Here’s a brief overview of each form:

1. **get_trucking1**:
- This form retrieves data from the `vekp` and `vepo` tables based on the `venum` field, which is matched against entries in the `lt_truk` internal table.
- It populates the `ls_vekp` structure with various fields from the `lt_hu` table and associates it with corresponding entries in `lt_truk`.
- The results are inserted into the `gt_vekp` internal table.

2. **fill_last_document**:
- This form checks if the `gt_input` internal table is not empty.
- It performs two SELECT statements:
- The first retrieves data from the `vbap` table based on the `vbeln` field from `gt_input`.
- The second retrieves data from the `ekpo` and `ekko` tables, joining them on the `ebeln` field, again based on the `vbeln` from `gt_input`.
- The results are stored in `gt_vbap` and `gt_ekpo` internal tables, respectively.

3. **solve_case**:
- This form determines the value of `pd_case` based on the presence of certain variables (`s_vbeln`, `s_truk`, `s_tknum`).
- It assigns a specific character ('D', 'T', or 'S') to `pd_case` depending on which variable is initialized.

If you have specific questions about any part of the code or need further clarification, feel free to ask!
It seems like you've provided a snippet of ABAP code that includes an `ENDIF` statement and a comment indicating the end of a form called `solve_case`. However, the snippet is incomplete, and there is no context or specific question provided.

If you have a specific question about this code or need assistance with a particular aspect of ABAP programming, please provide more details or clarify your request!

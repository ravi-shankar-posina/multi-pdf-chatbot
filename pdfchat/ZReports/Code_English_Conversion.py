import os
import nest_asyncio
nest_asyncio.apply()

from dotenv import load_dotenv
load_dotenv()

from llama_parse import LlamaParse
from langchain_community.document_loaders import DirectoryLoader

llamaparse_api_key = "llx-z0F58oYLQhKuctScPxJQZNz8VOy0B2qKVOlouOpyEXDL8iiT"
qdrant_url = "https://116d70ee-45c3-4953-9872-de0f93ea7615.us-east4-0.gcp.cloud.qdrant.io"
qdrant_api_key = "Dk2_xND7tAJe1HXKGrIq6lihtUF0t0XmoVV7G9Bfg7v-MEoZrodI6g"

import os
import pickle
import nltk

# Ensure necessary NLTK data is downloaded
nltk.download('punkt_tab')
nltk.download('averaged_perceptron_tagger_eng')

# Define the source and target directories
source_directory = r"C:\ABAPCode\Addtional_Codes\new codes"  # The directory where your documents are stored
target_directory = r"C:\ABAPCode\Addtional_Codes\additonal codes parsed"  # Directory to save the parsed data

# Create the target directory if it doesn't exist
if not os.path.exists(target_directory):
    os.makedirs(target_directory)

# Define the function to load parsed data or parse if not available
def load_or_parse_data(file_path):
    data_file = os.path.join(target_directory, f"{os.path.basename(file_path)}_parsed.pkl")
    
    if os.path.exists(data_file):
        # Load the parsed data from the file
        with open(data_file, "rb") as f:
            parsed_data = pickle.load(f)
    else:
        # Perform the parsing step
        parsingInstructionUber10k = """The provided documents contain different ABAP codes. 
        Try to be precise while answering the questions."""
        parser = LlamaParse(api_key=llamaparse_api_key, result_type="markdown", parsing_instruction=parsingInstructionUber10k)
        
        # Assuming you have a list of documents to parse (to_parse_documents should be defined)
        llama_parse_documents = parser.load_data([file_path])  # Change this if you're parsing multiple documents
        
        # Save the parsed data to the new folder
        with open(data_file, "wb") as f:
            pickle.dump(llama_parse_documents, f)
        
        # Set the parsed data to the variable
        parsed_data = llama_parse_documents
        print(f"Parsed data for {file_path}: {parsed_data}")
    
    return parsed_data

# Iterate over all files in the source directory and parse each file
for filename in os.listdir(source_directory):
    file_path = os.path.join(source_directory, filename)
    
    if os.path.isfile(file_path):  # Make sure it's a file
        print(f"Processing file: {filename}")
        parsed_data = load_or_parse_data(file_path)
        
        # Save the parsed data to individual markdown files named after the original filename
        md_filename = os.path.splitext(filename)[0] + ".md"  # Remove the extension and add .md
        md_file_path = os.path.join(target_directory, md_filename)
        
        with open(md_file_path, 'w') as f:  # Open the file in write mode ('w')
            for doc in parsed_data:
                f.write(doc.text + '\n')  # Write the text from parsed documents to the .md file

# Now load the markdown files from the target directory (you can adjust the glob pattern as needed)
loader = DirectoryLoader(target_directory, glob="**/*.md", show_progress=True)
documents = loader.load()

# Optionally print or process the loaded documents
for doc in documents:
    print(doc.text)  # You can replace this with any processing you need



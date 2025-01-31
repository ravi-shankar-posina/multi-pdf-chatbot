import json
import os
from crewai import Agent, Task, Crew

# Define agents
file_finder_agent = Agent(
    role='File Finder',
    goal='Find the ABAP markdown file based on user query.',
    backstory="You are an expert at locating files in directories.",
    allow_delegation=False,
)

abap_code_modifier_agent = Agent(
    role='ABAP Code Modifier',
    goal='Modify the given ABAP markdown file based on instructions.',
    backstory="You are skilled at modifying ABAP code in markdown files.",
    allow_delegation=False,
)

# Define tasks
file_finder_task = Task(
    description="Locate the ABAP markdown file in the 'parsed data' directory.",
    agent=file_finder_agent,
    expected_output="Path to the ABAP markdown file"
)

abap_code_modification_task = Task(
    description="Modify the ABAP code within the given markdown file.",
    agent=abap_code_modifier_agent,
    expected_output="Status of the modification operation"
)

# Define Crew
crew = Crew(
    agents=[file_finder_agent, abap_code_modifier_agent],
    tasks=[file_finder_task, abap_code_modification_task],
    verbose=True
)

def modify_abap_code(file_query, modification_request):
    """Executes the workflow to find a file and modify its ABAP code."""
    
    try:
        # Step 1: Find the File
        file_finder_result = crew.kickoff(inputs={"user_query": file_query})
        print("File Finder Result:", file_finder_result)
        
        # Construct the full file path
        base_dir = 'parsed data'
        file_path = os.path.join(base_dir, f"{file_query}.md")
        
        if not os.path.exists(file_path):
            # Try alternative naming or search in the directory
            for filename in os.listdir(base_dir):
                if file_query.lower() in filename.lower():
                    file_path = os.path.join(base_dir, filename)
                    break
        
        if not os.path.exists(file_path):
            print(f"Error: File {file_path} not found.")
            return {"status": "error", "message": f"File not found: {file_query}"}
        
        # Step 2: Modify the ABAP Code
        modification_result = crew.kickoff(
            inputs={"file_path": file_path, "modification_request": modification_request}
        )
        
        return modification_result
            
    except Exception as e:
        print(f"Error during execution: {str(e)}")
        return {"status": "error", "message": str(e)}
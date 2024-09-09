from flask import Flask, request, jsonify
import os
from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_community.document_loaders import PyPDFLoader
from langchain_community.document_loaders.csv_loader import CSVLoader
from langchain_text_splitters import RecursiveCharacterTextSplitter
from langchain_community.vectorstores import FAISS
from langchain.chains import RetrievalQA, create_retrieval_chain
from langchain.chains.combine_documents import create_stuff_documents_chain
from langchain_core.prompts import PromptTemplate, ChatPromptTemplate
from langchain_core.output_parsers import StrOutputParser
from langchain_core.messages import HumanMessage, SystemMessage
from dotenv import load_dotenv
import fitz

load_dotenv()

app = Flask(__name__)

### PDF Processing ###
# List of PDF file paths
pdf_file_paths = ["./Reference/12.SD-Sales Monitoring and Analytics.pdf",
                  "./Reference/13.SD-Special Business Processes in Sales.pdf",
                  "./Reference/14.SD-Integrations.pdf"]

# Load documents from multiple PDF files
all_pdf_docs = []
for file_path in pdf_file_paths:
    loader = PyPDFLoader(file_path=file_path)
    documents = loader.load()
    all_pdf_docs.extend(documents)

# Split PDF documents
pdf_text_splitter = RecursiveCharacterTextSplitter(chunk_size=1000, chunk_overlap=200)
split_pdf_docs = pdf_text_splitter.split_documents(all_pdf_docs)

# Embeddings and FAISS vector store for PDFs
pdf_embeddings = OpenAIEmbeddings(model="text-embedding-3-large")
pdf_vectordb_file_path = "./StoreFAISSPDF"
if os.path.exists(pdf_vectordb_file_path):
    pdf_vectordb = FAISS.load_local(pdf_vectordb_file_path, embeddings=pdf_embeddings, allow_dangerous_deserialization=True)
else:
    pdf_vectordb = FAISS.from_documents(documents=split_pdf_docs, embedding=pdf_embeddings)
    pdf_vectordb.save_local(folder_path=pdf_vectordb_file_path)

pdf_retriever = pdf_vectordb.as_retriever(search_type="similarity", search_kwargs={"k": 3})

# Define prompt template for PDF RetrievalQA
pdf_prompt_template = """You are a PDF Reader AI Assistant. Your task is to take in the user query and generate the answer only from the context provided.
Try to provide a detailed answer looking at all the retrieved context.
However, you have to stick to the context provided strictly. Nothing in the answer should be from outside of the context formulated.
If you can't find the answer in the context, just say that you don't know, don't try to make up an answer.

CONTEXT: {context}

QUESTION: {question}"""

PDF_PROMPT = PromptTemplate(template=pdf_prompt_template, input_variables=["context", "question"])

# Define and initialize RetrievalQA chain for PDFs
pdf_chain = RetrievalQA.from_chain_type(
    llm=ChatOpenAI(model="gpt-4"),
    chain_type="stuff",
    retriever=pdf_retriever,
    input_key="query",
    return_source_documents=True,
    chain_type_kwargs={"prompt": PDF_PROMPT}
)

def load_and_extract_images(pdf_path, page_num, output_dir):
    document = fitz.open(pdf_path)
    all_images = []
    images = extract_images_from_pdf_with_pagenum(document, page_num, output_dir)
    all_images.extend(images)
    return all_images

def extract_images_from_pdf_with_pagenum(document, page_num, output_dir):
    if not os.path.exists(output_dir):
        os.makedirs(output_dir)

    page = document.load_page(page_num)
    image_list = page.get_images(full=True)

    images_info = []
    for img_index, img in enumerate(image_list):
        xref = img[0]
        pix = fitz.Pixmap(document, xref)
        rect = fitz.Rect(pix.irect)
        text = page.get_text("text", clip=rect)
        if not text:
            continue

        base_image = document.extract_image(xref)
        image_bytes = base_image["image"]
        img_ext = base_image["ext"]
        image_filename = f"image_{page_num + 1}_{img_index + 1}.{img_ext}"
        image_path = os.path.join(output_dir, image_filename)

        with open(image_path, "wb") as image_file:
            image_file.write(image_bytes)

        image_info = {
            'page_number': page_num + 1,
            'image_path': image_path,
            'text': text.strip()
        }
        images_info.append(image_info)
    return images_info

def extract_all_related_links_with_specific_page(file_path, page_num):
    document = fitz.open(file_path)
    links_info = []

    page = document.load_page(page_num)
    links = page.get_links()
    for link in links:
        if 'uri' in link:
            rect = link['from']
            text = page.get_text("text", clip=rect)
            link_info = {
                'page_number': page_num + 1,
                'link': link['uri'],
                'text': text.strip()
            }
            links_info.append(link_info)

    return links_info

def ask_pdf_question(question):
    if not question:
        return {"error": "No question provided"}

    # Process the question using the PDF QA chain
    response = pdf_chain.invoke({"query": question})

    # Check if the answer is "I don't know"
    if response["result"].strip().lower() == "i don't know." or response["result"].strip().lower().startswith("i'm sorry,"):
        return {"answer": "I couldn't find the specific answer to your query. Please try again with a different question."}

    # Retrieve relevant documents
    top_k_docs = pdf_retriever.get_relevant_documents(question)
    sources_content = []
    pages_content = []
    sources_content_pages = set()

    for source in response['source_documents']:
        sources_content.append(source.metadata['source'])
        pages_content.append(source.page_content)
        entry = (source.metadata['source'], source.metadata['page'])
        sources_content_pages.add(entry)

    image_info = []
    link_info = []

    output_dir = "./ImagesExtracted"

    for file_path, page_num in sources_content_pages:
        # Extract images
        images = load_and_extract_images(file_path, page_num, output_dir)
        for eachimage in images:
            image_info.append({
                "page_number": eachimage["page_number"],
                "image_path": eachimage["image_path"],
                "text": eachimage["text"]
            })

        # Extract links
        links = extract_all_related_links_with_specific_page(file_path, page_num)
        for eachlink in links:
            link_info.append({
                "page_number": eachlink["page_number"],
                "link": eachlink["link"],
                "text": eachlink["text"]
            })

    result = {
        "answer": response["result"],
        "sources": sources_content,
        "additional_info": pages_content
    }

    if image_info:
        result["image_info"] = image_info
    if link_info:
        result["link_info"] = link_info

    return result


### CSV Processing ###
csv_file_path = "./HowToDataStore.csv"
csv_loader = CSVLoader(file_path=csv_file_path, encoding='latin-1')
csv_data = csv_loader.load()

csv_text_splitter = RecursiveCharacterTextSplitter(chunk_size=1000, chunk_overlap=200, add_start_index=True)
split_csv_docs = csv_text_splitter.split_documents(csv_data)

# Embeddings and FAISS vector store for CSV
csv_embeddings = OpenAIEmbeddings(model="text-embedding-3-large")
csv_vectordb_file_path = "./StoreFAISSCsv"
if os.path.exists(csv_vectordb_file_path):
    csv_vectordb = FAISS.load_local(csv_vectordb_file_path, embeddings=csv_embeddings, allow_dangerous_deserialization=True)
else:
    csv_vectordb = FAISS.from_documents(documents=split_csv_docs, embedding=csv_embeddings)
    csv_vectordb.save_local(folder_path=csv_vectordb_file_path)

csv_retriever = csv_vectordb.as_retriever(search_type="similarity", search_kwargs={"k": 3})

# Define prompts for CSV Retrieval
csv_system_prompt = (
    "You are an assistant for generating the responses for CSV prompts. "
    "Use the following pieces of retrieved context to answer the question. "
    "If the answer is not present, say that: I don't know. "
    "Provide a full, detailed answer."
    "\n\n"
    "{context}"
)

csv_chat_prompt = ChatPromptTemplate.from_messages(
    [
        ("system", csv_system_prompt),
        ("human", "{input}"),
    ]
)

# Create CSV retrieval chain
csv_question_answer_chain = create_stuff_documents_chain(ChatOpenAI(model="gpt-4"), csv_chat_prompt)
csv_rag_chain = create_retrieval_chain(csv_retriever, csv_question_answer_chain)


def ask_csv_question(user_query):
    if not user_query:
        return {"error": "No query provided"}

    # Retrieve top k documents
    top_k_docs = csv_retriever.get_relevant_documents(user_query)

    # Convert the documents to a serializable format
    sources = []
    for doc in top_k_docs:
        sources.append({
            "content": doc.page_content,  # assuming the document has `page_content`
            "metadata": doc.metadata      # assuming the document has `metadata`
        })

    # Use the query in the CSV chain to generate the final answer
    try:
        response = csv_rag_chain.invoke({"input": user_query})
        
        # Check if 'output' exists in the response
        if "output" not in response:
            return {
                "error": "No output found in the response",
                "sources": sources
            }

        result = {
            "answer": response["output"],
            "sources": sources
        }

    except Exception as e:
        # Handle any unexpected errors during invocation
        result = {
            "error": f"An error occurred: {str(e)}",
            "sources": sources
        }

    return result


@app.route("/pdf/query", methods=["POST"])
def query_pdf():
    user_query = request.json.get("query")
    result = ask_pdf_question(user_query)
    return jsonify(result)


@app.route("/csv/query", methods=["POST"])
def query_csv():
    user_query = request.json.get("query")
    result = ask_csv_question(user_query)
    return jsonify(result)


if __name__ == "__main__":
    app.run(debug=True)
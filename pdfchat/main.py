from flask import Flask, request, jsonify
from flask_cors import CORS
from dotenv import load_dotenv
from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_google_genai import ChatGoogleGenerativeAI, GoogleGenerativeAIEmbeddings
from langchain_core.prompts import ChatPromptTemplate, MessagesPlaceholder
from langchain.chains.combine_documents import create_stuff_documents_chain
from langchain.chains import create_retrieval_chain
from langchain_core.messages import HumanMessage, AIMessage
from langchain.chains.history_aware_retriever import create_history_aware_retriever
from langchain_qdrant import Qdrant
import os

app = Flask(__name__)
CORS(app, origins="*")

load_dotenv()
model_config = {
    "openai": {
        "name" : "OpenAI",
        "llm": lambda: ChatOpenAI(model="gpt-4o-mini", temperature=0.5),
        "embeddings": lambda: OpenAIEmbeddings(),
        "collection_csv": os.getenv("QDRANT_CSV_OPENAI_COLLECTION"),
        "collection_pdf": os.getenv("QDRANT_PDF_OPENAI_COLLECTION"),
    },
    "gemini": {
        "name" : "Google AI",
        "llm": lambda: ChatGoogleGenerativeAI(model="gemini-1.5-pro", temperature=0.5),
        "embeddings": lambda: GoogleGenerativeAIEmbeddings(model="models/embedding-001"),
        "collection_csv": os.getenv("QDRANT_CSV_GEMINI_COLLECTION"),
        "collection_pdf": os.getenv("QDRANT_PDF_GEMINI_COLLECTION"),
    }
}

chat_history = []

def get_model_config(model_name):
    config = model_config.get(model_name)
    if not config:
        return None, None, None, None
    llm = config["llm"]()
    embeddings = config["embeddings"]()
    collection_csv = config["collection_csv"]
    collection_pdf = config["collection_pdf"]
    return llm, embeddings, collection_csv, collection_pdf

llm, instructor_embeddings, collection_name_csv, collection_name_pdf = get_model_config("openai")
url = os.getenv("QDRANT_URL")

def create_chain(collection_name):
    vectorStore = Qdrant.from_existing_collection(
                    embedding=instructor_embeddings,
                    collection_name=collection_name,
                    url=url,
                )
    prompt = ChatPromptTemplate.from_messages([
        ("system", "You are a friendly assistant who gives human like responses. Given the following context and a question, generate an answer based on this context as much as possible. Give detailed answers. at least 100 words. In markdown format with paragraphs, headings and bullet points. In the answer, try to provide as much text as possible from the \"response\" section in the source document context without making many changes. If the information is not available in the context, you may then search the internet to provide an answer. Don't ask for confirmation. Just give the answer.: {context}"),
        MessagesPlaceholder(variable_name="chat_history"),
        ("human", "{input}")
    ])
    chain = create_stuff_documents_chain(
        llm=llm,
        prompt=prompt,
    )
    retriever = vectorStore.as_retriever(score_threshold=0.7)
    retriever_prompt = ChatPromptTemplate.from_messages([
        MessagesPlaceholder(variable_name="chat_history"),
        ("human", "{input}"),
        ("human", "Given the above conversation, generate a search query to look up in order to get information relevant to the conversation")
    ])
    history_aware_retriever = create_history_aware_retriever(
        llm=llm,
        retriever=retriever,
        prompt=retriever_prompt
    )
    retrieval_chain = create_retrieval_chain(
        history_aware_retriever,
        chain,
    )
    return retrieval_chain

def process_chat(chain, question, chat_history):
    response = chain.invoke({
        "input": question,
        "chat_history": chat_history
    })
    return response

@app.route('/pdfchat', methods=['POST'])
def pdf_chat():
    global chat_history
    data = request.get_json()
    question = data['prompt']
    response = process_chat(pdf_chain, question, chat_history)
    chat_history.append(HumanMessage(content=question))
    chat_history.append(AIMessage(content=response["answer"]))
    return response["answer"]

@app.route('/csvchat', methods=['POST'])
def csv_chat():
    global chat_history
    data = request.get_json()
    question = data['prompt']
    response = process_chat(csv_chain, question, chat_history)
    chat_history.append(HumanMessage(content=question))
    chat_history.append(AIMessage(content=response["answer"]))
    return response["answer"]

@app.route('/change_model', methods=['POST'])
def change_model():
    global llm, instructor_embeddings, collection_name_csv, collection_name_pdf, pdf_chain, csv_chain
    data = request.get_json()
    model = data['model']
    llm, instructor_embeddings, collection_name_csv, collection_name_pdf = get_model_config(model)
    if not llm:
        return jsonify({"message": "Invalid model"}), 400
    pdf_chain = create_chain(collection_name_pdf)
    csv_chain = create_chain(collection_name_csv)
    return jsonify({"message": "Model changed successfully"})

@app.route('/get_models', methods=['GET'])
def get_models():
    return jsonify({
        "models": [
            {"value": model, "name": model_config[model]["name"]}
            for model in model_config
        ]
    })

if __name__ == '__main__':
    pdf_chain = create_chain(collection_name_pdf)
    csv_chain = create_chain(collection_name_csv)
    app.run(host='0.0.0.0', port=8501, debug=True)
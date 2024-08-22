from flask import Flask, request, jsonify
from flask_cors import CORS
from dotenv import load_dotenv
from langchain_openai import ChatOpenAI, OpenAIEmbeddings
from langchain_core.prompts import ChatPromptTemplate
from langchain.chains.combine_documents import create_stuff_documents_chain
from langchain.chains import create_retrieval_chain
from langchain_community.vectorstores.faiss import FAISS
import os

app = Flask(__name__)
CORS(app, origins="*")

load_dotenv()

llm = ChatOpenAI(model="gpt-4o-mini", temperature=0.5)

def load_faiss_db(db_path):
    return FAISS.load_local(db_path, OpenAIEmbeddings(), allow_dangerous_deserialization=True)

def create_chain(vectorStore):
    prompt = ChatPromptTemplate.from_template("""
    Answer the user's question:
    Context: {context}
    Question: {input}                                       
    """)
    
    chain = create_stuff_documents_chain(llm=llm, prompt=prompt)
    retriever = vectorStore.as_retriever(search_kwargs={"k": 1})
    return create_retrieval_chain(retriever, chain)

@app.route('/pdfchat', methods=['POST'])
def pdf_chat():
    data = request.get_json()
    question = data['prompt']
    response = process_chat(pdf_chain, question)
    sources = [{"page_content": doc.page_content, "metadata": doc.metadata} for doc in response["context"]]
    return jsonify({"answer": response["answer"], "source": sources[0]["metadata"]["source"]})

@app.route('/csvchat', methods=['POST'])
def csv_chat():
    data = request.get_json()
    question = data['prompt']
    response = process_chat(csv_chain, question)
    sources = [{"page_content": doc.page_content, "metadata": doc.metadata} for doc in response["context"]]
    return jsonify({"answer": response["answer"], "source": sources[0]["metadata"]["source"]})

def process_chat(chain, question):
    response = chain.invoke({"input": question})
    return response

if __name__ == '__main__':
    pdf_vectorStore = load_faiss_db("faiss_pdf_index")
    csv_vectorStore = load_faiss_db("faiss_csv_index")

    pdf_chain = create_chain(pdf_vectorStore)
    csv_chain = create_chain(csv_vectorStore)

    app.run(host='0.0.0.0', port=8501, debug=True)
FROM python:3.10

WORKDIR /app

COPY requirements.txt .

RUN pip install --no-cache-dir -r requirements.txt

COPY . .

RUN python create_db.py

EXPOSE 8501

CMD ["python", "app.py"]
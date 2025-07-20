FROM python:3.10

# Rajesh

WORKDIR /app

COPY requirements.txt .

RUN pip install --no-cache-dir -r requirements.txt

COPY . .

EXPOSE 8502

CMD ["python", "main.py"]
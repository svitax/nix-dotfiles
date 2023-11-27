from flask import send_file
from flask import Flask
from io import BytesIO
from PIL import Image
import requests
import numpy as np
import pandas as pd
import torch


app = Flask(__name__)


IMAGE_URL = "https://art.afsheen.info/img/2017/2014_04_stitch001.jpg"
IMAGE_SIZE = (300, 300)


@app.route("/")
def hello():
    return "Hello World!"


@app.route("/image")
def image():
    r = requests.get(IMAGE_URL)
    if not r.status_code == 200:
        raise ValueError(f"Response code was '{r.status_code}'")

    img_io = BytesIO()

    img = Image.open(BytesIO(r.content))
    img.thumbnail(IMAGE_SIZE)
    img.save(img_io, "JPEG", quality=70)

    img_io.seek(0)

    return send_file(img_io, mimetype="image/jpeg")


def main():
    app.run()

    a = np.array([1, 2, 3], dtype="int8")
    b = np.array([[1.0, 2.0, 3.0], [4.0, 5.0, 6.0]])
    print(a)
    print(b)
    print(pd.DataFrame({}))

    data = [[1, 2, 3, 4], [3, 4, 5, 6]]
    x_data = torch.tensor(data)
    x_rand = torch.rand_like(x_data, dtype=torch.float)
    print(x_rand)


if __name__ == "__main__":
    main()

import requests

def upload_png_image(url: str, image_path: str):
    with open(image_path, 'rb') as data:
        headers = {
            'Content-Type': 'image/png'
        }
        return requests.post(url, data=data, headers=headers)

response1 = upload_png_image('http://localhost:8080/uploadMedia?fileName=1.png', 'test/1.png')
print('Image 1:', response1.status_code, response1.text)

response2 = upload_png_image('http://localhost:8080/uploadMedia?fileName=2.png', 'test/2.png')
print('Image 2:', response2.status_code, response2.text)

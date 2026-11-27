from pathlib import Path

from PIL import Image, ImageDraw, ImageFont


def convert_ascii_to_image(ascii_file, output_image):
    with open(ascii_file, "r") as file:
        ascii_art = file.read()

    ImageFont.truetype
    font = ImageFont.truetype("JuliaMono-Light.ttf", 16)
    dummy_image = Image.new("RGB", (1, 1))
    draw = ImageDraw.Draw(dummy_image)

    bbox = draw.textbbox((0, 0), ascii_art, font=font)
    width = int(bbox[2])
    height = int(bbox[3])

    image = Image.new("RGB", (width, height), color="white")
    draw = ImageDraw.Draw(image)
    draw.text((0, 0), ascii_art, fill="black", font=font)

    image.save(output_image)


files = sorted(Path(".").glob("map-at-*.txt"))
total = len(files)
for index, text_file in enumerate(files):
    print(f"Converting {index} of {total}")
    convert_ascii_to_image(text_file, f"frame_{index:05d}.png")

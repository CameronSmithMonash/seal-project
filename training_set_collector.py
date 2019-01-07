from PIL import Image
from io import BytesIO
from math import floor, sqrt
import csv
import requests
import random


# function which returns the rank of user from a row of the best_classifications matrix
def id_rank(matrix_row):
    current_user = matrix_row[1]
    if current_user in best_users:
        return best_users.index(current_user)
    else:
        return len(best_users)


# function that returns the rank of used based on when the image appears in best_classifications
def image_rank(matrix_row):
    current_image = matrix_row[0]
    try:
        return best_image_list.index(current_image)
    # if current_image is not in best_image_list
    except ValueError:
        return len(best_image_list)


# finds the url based on the image file name, width, height and location
def url_finder(img_file, img_width, img_height, img_location):
    img_url = "http://43.240.99.95/sealSpotter_data-20181115/images/"
    number = img_file[:8]
    folder = number + "_" + img_location
    img_url += folder + "/"

    # find position of dimensions (x[width]_y[height]) in image string
    i = -1
    character = img_file[i]
    while character != "x":
        i -= 1
        character = img_file[i]

    if folder == "20180105_deenMaar":
        # deenMaar has a sub folder
        sub_folder = img_file[:i] + "sliced_x" + str(img_width) + "_y" + str(img_height)
        img_url += sub_folder + "/"

        # typo in url (dignhy instead of dinghy)
        if img_url == 'http://43.240.99.95/sealSpotter_data-20181115/images/20180105_deenMaar/' \
                      '20180105-deenMaar-DinghyCove_sliced_x1200_y1000/':
            img_url = 'http://43.240.99.95/sealSpotter_data-20181115/images/20180105_deenMaar/' \
                  '20180105-deenMaar-DignhyCove_sliced_x1200_y1000/'

        img_url += img_file

    elif folder == "20180121_gaboIsland":
        img_url += folder + "_cropped_slicedMosaic_" + img_file[i:]
        return img_url

    elif folder == "20171122_sealRocks":
        img_url += number + "_" + img_file[i:]
        return img_url

    elif folder == "20180103_marengoReef":
        img_url += number + "-marengoReef_croppedRegion_" + img_file[i:]

    else:
        img_url += folder + "_" + img_file[i:]

    return img_url


# get list of best users from user_accuracy.csv
best_users = []
with open('R/user_rankings.csv') as file:
    reader = csv.reader(file)
    for row in reader:
        # if not the first row (row of column names)
        if row[0] != "user":
            # if variation is less then1 5%:
            if float(row[2]) < 15:
                best_users.append(int(row[0]))

# put information from 20181115-sealSpotter_classifications_anonymised.csv into matrix with the following columns:
#  imagefile, user_anonymised, type, pointsImageX, pointsImageY, imgWidth, imgHeight, location

# classifications by best users
classifications = []
with open('20181115-sealSpotter_classifications_anonymised.csv') as file:
    reader = csv.reader(file)
    # ignore first row (column names)
    next(reader)
    # iterate through rows of reader, and adding information for each
    image = ""
    user = ""
    width = int
    height = int
    location = ""
    for row in reader:
        classification_type = row[5]
        x_coord = float(row[6])
        y_coord = float(row[7])

        # if type is not "comment" and image does not include fullMosaic (these were not drone pictures)
        if (classification_type != "comment") and ("fullMosaic" not in row[2]):
            if image == row[2] and user == int(row[3]):
                classifications[-1][-1].append((x_coord, y_coord))

            else:
                image = row[2]
                user = int(row[3])
                width = int(row[8])
                height = int(row[9])
                location = row[11]
                # add information to classifications matrix
                new_row = (image, user, classification_type, width, height, location, [(x_coord, y_coord)])

                classifications.append(new_row)


# sort best_classifications based on user id ranking
classifications.sort(key=id_rank)

best_classifications = []
other_classifications = []
image_list = []
for row in classifications:
    image = row[0]
    user = row[1]
    classification_type = row[2]
    if user in best_users and image not in image_list:
        best_classifications.append(row)
        image_list.append(image)
    else:
        other_classifications.append(row)


# get list of images in same order as in best_classifications
best_image_list = []
for row in best_classifications:
    if row[0] not in best_image_list:
        best_image_list.append(row[0])


# sort other_classifications based on when image first appears in best_classifications
other_classifications.sort(key=image_rank)


# write to csv (this was used when testing)
with open("best_user_classifications.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerows(best_classifications)

with open("other_user_classifications.csv", "w") as f:
    writer = csv.writer(f)
    writer.writerows(other_classifications)


# create blank file to store images with no url
open('no_url.txt', 'w').close()


# go through images based on ranked user
# -for each seal identified, make clipping of size n x n around point
# -rotate clipping and save to seals folder
# -find Seal coordinates from every user who classified image
# -randomly pick a point on the image
# -if point is at least m pixels away from every seal point make a clipping around point
# -rotate clipping and save to no_seals folder
n = int(input("Enter value for n: "))
seal_count = 0
no_seal_count = 0
# current index in other_classifications
index = 0
current_other_classifications_row = other_classifications[index]
for row in best_classifications:
    image_name = row[0]
    user = row[1]
    classification_type = row[2]
    width = row[3]
    height = row[4]
    location = row[5]
    coordinates = row[6]
    url = url_finder(image_name, width, height, location)
    response = requests.get(url)

    # some images in spreadsheet did not appear in the database
    # therefore, try-except is used
    try:
        img = Image.open(BytesIO(response.content))
        if classification_type != "zerocount":
            for item in coordinates:
                # crop image around coordinates
                x = item[0]
                y = item[1]
                current_img = img.crop((x-n, height-y-n, x+n, height - y+n))

                # random rotation
                random_rotation = random.random()
                if 0.25 < random_rotation < 0.5:
                    current_img.transpose(Image.ROTATE_90)
                elif 0.5 < random_rotation < 0.75:
                    current_img.transpose(Image.ROTATE_180)
                elif 0.75 < random_rotation:
                    current_img.transpose(Image.ROTATE_270)

                # save image
                current_img.save("seal_images/seal" + str(seal_count) + ".jpg")
                seal_count += 1

            while current_other_classifications_row[index] == image_name:
                other_coordinates = current_other_classifications_row[index]
                coordinates += other_coordinates
                index += 1

        # if type == zerocount
        else:
            coordinates = []
            # add in coordinates form other users
            while current_other_classifications_row[index] == image_name:
                other_coordinates = current_other_classifications_row[index]
                coordinates += other_coordinates
                index += 1

        # while there are less images of non_seals then seals,
        # randomly pick coordinates, and if it is not too close to seals (based on user coordinates),
        # get a clipping of it
        # if coordinate is too close to identified seals 5 times, cancel
        near_seal_count = 0
        while no_seal_count < seal_count and near_seal_count < 5:
            x_coord = floor(random.random()*width)
            y_coord = floor(random.random()*width)
            near_seal = False
            for item in coordinates:
                x = item[0]
                y = item[1]
                dist = sqrt((x - x_coord) ** 2 + (y - y_coord) ** 2)
                if dist < 2*n:
                    near_seal = True

            if not near_seal:
                current_img = img.crop((x_coord - n, height - y_coord - n, x_coord + n, height - y_coord + n))

                # random rotation
                random_rotation = random.random()
                if 0.25 < random_rotation < 0.5:
                    current_img.transpose(Image.ROTATE_90)
                elif 0.5 < random_rotation < 0.75:
                    current_img.transpose(Image.ROTATE_180)
                elif 0.75 < random_rotation:
                    current_img.transpose(Image.ROTATE_270)

                # save image
                current_img.save("no_seal_images/no_seal" + str(seal_count) + ".jpg")
                no_seal_count += 1

            else:
                near_seal_count += 1

    except OSError:
        with open("no_url.txt", "a") as f:
            f.write("name: " + image_name + " predicted url: " + url + "\n")

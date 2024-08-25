home_team = 'Alabama'



from io import BytesIO
from PIL import Image
import urllib
import PIL
import sportsdataverse.nfl as nfl
import numpy as np
import pandas as pd

teams = pd.read_csv('team_info.csv')
teams.index = teams.school

teams.logo
import matplotlib.pyplot as plt
import requests
# create a file-like object from the url
url = teams.loc[home_team,'logo']
img = np.array(PIL.Image.open(urllib.request.urlopen(url)))
#response = requests.get(f'{team_info.team_wordmark[0]}')
#img = Image.open(BytesIO(response.content))
#image = Image.open(urllib.request.urlopen(f'{team_info.team_wordmark[0]}'))
# read the image fplt.imshow(img)

plt.imshow(img)
plt.axis('off')
plt.show()

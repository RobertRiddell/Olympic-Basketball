import pandas as pd
import datetime
from dateutil.parser import parse
from requests.api import request
import requests
from bs4 import BeautifulSoup
from bs4 import Comment

# https://stackoverflow.com/questions/57032340/python-beautiful-soup-cant-find-specific-table

dat = pd.read_csv("data/fixtures/2000-olympic-fixtures.csv")
dt = parse(dat['Date'][1])
opp = dat['Opp'][1]
opp = opp.lower()
home_team = dat['Team'][1]
home_team = home_team.lower()
dt = datetime.datetime.strftime(dt, '%Y-%m-%d')
URL = f'https://www.basketball-reference.com/international/boxscores/{dt}-{opp}.html'
response = requests.get(URL)
soup = BeautifulSoup(response.text, 'html.parser')
comments = soup.find_all(string=lambda text: isinstance(text, Comment))
tables = []
for each in comments:
    if 'table' in each:
        try:
            tables.append(pd.read_html(each)[0])
        except:
            continue

home = tables[0]
away = tables[1]

home['Country'] = home_team
away['Country'] = opp
home['Date'] = dt
away['Date'] = dt
both = [home, away]

complete = pd.concat(both)
complete.to_csv(f'{dt}_{home_team}_{opp}.csv')
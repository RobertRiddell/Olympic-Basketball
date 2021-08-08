from numpy.lib import ufunclike
import pandas as pd
import datetime
from dateutil.parser import parse
from requests.api import request
import requests
from bs4 import BeautifulSoup
from bs4 import Comment
import os

def olympic_boxscores(competition, year):
    dat = pd.read_csv(f'data/{competition}/fixtures/{year}-{competition}-fixtures.csv')
    dat.rename(columns={list(dat)[4]:'tournament_stage'}, inplace=True)
    for i in range(len(dat)):
        dt = parse(dat['Date'][i])
        opp = dat['Opp'][i]
        opp = opp.lower()
        opp = opp.replace(" ", "-")
        home_team = dat['Team'][i]
        home_team = home_team.lower()
        dt = datetime.datetime.strftime(dt, '%Y-%m-%d')
        URL = f'https://www.basketball-reference.com/international/boxscores/{dt}-{opp}.html'
        # https://stackoverflow.com/questions/57032340/python-beautiful-soup-cant-find-specific-table
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

        id = (f'{dt}-{str(i)}')
        id = id.replace("-","")



        home['Country'] = home_team
        away['Country'] = opp
        home['Date'] = dt
        away['Date'] = dt
        home['Opposition'] = opp
        away['Opposition'] = home_team
        home['id'] = id
        away['id'] = id
        home['tournament_stage'] = dat['tournament_stage'][i]
        away['tournament_stage'] = dat['tournament_stage'][i]
        both = [home, away]
        
        complete = pd.concat(both)
        complete['game_number'] = f'{year}{i}'
        print(competition, year)
        
        path = f'data/{competition}/stats/{year}'
        
        if os.path.isdir(path):
            complete.to_csv(f'{path}/{dt}_{home_team}_{opp}.csv')
        else:
            os.mkdir(path)
            complete.to_csv(f'{path}/{dt}_{home_team}_{opp}.csv')

dict = {"fiba-world-cup": ['2010','2014','2019'], "mens-olympics": ['2000','2004','2008','2012','2016']}

for key in dict.keys():
   for item in range(len(dict[key])):
        olympic_boxscores(key, (dict[key][item]))


from numpy.lib import ufunclike
import pandas as pd
import datetime
from dateutil.parser import parse
from requests.api import request
import requests
from bs4 import BeautifulSoup
from bs4 import Comment
import os

def olympic_boxscores(year):
    dat = pd.read_csv(f'data/fixtures/{year}-olympic-fixtures.csv')
    dat.rename(columns={list(dat)[4]:'tournament_stage'}, inplace=True)
    game_number = 1
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
        home_points = int(home.loc[home['Player'] == 'Totals']['PTS'])
        away_points = int(away.loc[away['Player'] == 'Totals']['PTS'])

        if home_points > away_points:
            home['Result'] = "W"
            away['Result'] = "L"
        elif home_points == away_points:
            home['Result'] = "D"
            away['Result'] = "D"
        else:
            home['Result'] = "L"
            away['Result'] = "L"

        both = [home, away]
        
        
        complete = pd.concat(both)
        path = os.path.join('data/stats/',year)
        if os.path.isdir(path):
            complete.to_csv(f'{path}/{dt}_{home_team}_{opp}.csv')
        else:
            os.mkdir(path)
            complete.to_csv(f'{path}/{dt}_{home_team}_{opp}.csv')

years = ['2000','2004','2008','2012','2016']

for y in years:
    olympic_boxscores(y)



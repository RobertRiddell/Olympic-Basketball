import requests, json
from bs4 import BeautifulSoup
import pandas as pd
import os
import lxml.html as lh
import pandas as pd

def olympic_fixtures(year):
    URL = (f"https://www.basketball-reference.com/international/mens-olympics/{year}-schedule.html#games")
    res = requests.get(URL)
    
    # code from https://towardsdatascience.com/web-scraping-html-tables-with-python-c9baba21059
    doc = lh.fromstring(res.content)
    tr_elements = doc.xpath('//tr')
    
    col=[]
    i=0

    for t in tr_elements[0]:
        i+=1
        name=t.text_content()
        col.append((name,[]))
      
    for j in range(1,len(tr_elements)):
        #T is our j'th row
        T=tr_elements[j]
        
        #If row is not of size 9, the //tr data is not from our table 
        
        if len(T)!=9:
            break
        
        #i is the index of our column
        i=0
        
        #Iterate through each element of the row
        for t in T.iterchildren():
            data=t.text_content() 
            #Check if row is empty
            if i>0:
            #Convert any numerical value to integers
                try:
                    data=int(data)
                except:
                    pass
            #Append the data to the empty list of the i'th column
            col[i][1].append(data)
            #Increment i for the next column
            i+=1



    Dict={title:column for (title,column) in col}
    df=pd.DataFrame(Dict)
    df.to_csv(f"data/fixtures/{year}-olympic-fixtures.csv")

years = ['2000','2004','2008','2012','2016']

for i in(years):
   olympic_fixtures(i)

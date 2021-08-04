import pandas as pd
import datetime
from dateutil.parser import parse
from requests.api import request
import requests
import lxml.html as lh

dat = pd.read_csv("data/fixtures/2000-olympic-fixtures.csv")
## https://www.basketball-reference.com/international/boxscores/2016-08-06-france.html
dt = parse(dat['Date'][1])
dt = datetime.datetime.strftime(dt, '%Y-%m-%d')
URL = f'https://www.basketball-reference.com/international/boxscores/{dt}-france.html'
res = requests.get(URL)
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
        
    #If row is not of size 10, the //tr data is not from our table 
        
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
print(df)
#df.to_csv(f"data/fixtures/{year}-olympic-fixtures.csv")
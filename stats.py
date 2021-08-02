import requests, json
from bs4 import BeautifulSoup
import pandas as pd
import os
import lxml.html as lh
import pandas as pd

URL = "https://www.basketball-reference.com/international/mens-olympics/2016-schedule.html#games"
res = requests.get(URL)

doc = lh.fromstring(res.content)

tr_elements = doc.xpath('//tr')

col=[]
i=0
#For each row, store each first element (header) and an empty list
for t in tr_elements[0]:
    i+=1
    name=t.text_content()
    print(i,name)
    col.append((name,[]))

print(res.json)
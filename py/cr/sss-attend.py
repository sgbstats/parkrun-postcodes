import requests
from bs4 import BeautifulSoup
import json
import time
import os
import pandas as pd

headers = {
    'User-Agent': "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/135.0.0.0 Safari/537.36"
}

endcliffe = pd.read_csv('py/junk/endcliffe.csv')

i=0
empty_parkruns = []

for parkrun_name in endcliffe['name']:
  url = "https://www.parkrun.org.uk/" + parkrun_name.replace(" ", "").lower() + "/groups/2988/"
  
  # Send a GET request to the URL
  response = requests.get(url, headers=headers)

  # Check if the request was successful
  if response.status_code == 200:
    # Parse the HTML content
    soup = BeautifulSoup(response.text, 'html.parser')
    table = soup.find("table", id="results")

    if table:
      data = []
      for row in table.find_all("tr"):
          cols = row.find_all("td")
          if cols:
              runner_name = cols[0].get_text(strip=True)
              local_runs = cols[2].get_text(strip=True)
              total_runs = cols[3].get_text(strip=True)
              data.append([runner_name, local_runs, total_runs])

      # Convert to DataFrame
      df = pd.DataFrame(data, columns=["Runner", "localparkruns", "totalparkruns"])
      print(parkrun_name, len(df))
      
      if df.empty:
        i += 1
        empty_parkruns.append(parkrun_name)
      
      if i == 5:
        break


  if response.status_code != 200:
    print(f"Error at {parkrun_name}")
    exit()


  # Sleep for 30 seconds before fetching the next event
  # print(f"{parkrun_name}")
  time.sleep(30) # you can customise this if you want

print(empty_parkruns)

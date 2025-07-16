import requests
from bs4 import BeautifulSoup
import json
import time
import os

# URL of the page to scrape
events_url = "https://images.parkrun.com/events.json"

headers = {
    'User-Agent': "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/135.0.0.0 Safari/537.36"
}

events_fetch = requests.get(events_url, headers=headers)

# Save the JSON response to a file
if events_fetch.status_code == 200:
  with open("events.json", "w") as file:
    file.write(events_fetch.text)
  events = events_fetch.json()
else:
  print(f"Failed to fetch events JSON. Status code: {events_fetch.status_code}")

# # Open and load the events.json file
# with open("events.json", "r") as file:
#   events_data = json.load(file)

# Filter events with countrycode 97
filtered_events = [
  event for event in events.get("events").get("features", [])
  if event.get("properties", {}).get("countrycode") == 97
]

# Print the filtered events
for event in filtered_events:
  name = event.get("properties", {}).get("eventname")

  if "junior" in name.lower():
    continue

  # Check if the file already exists
  file_path = f"py/data/{name}_event_history.html"
  if os.path.exists(file_path):
    print(f"Skipping {name} as the file already exists.")
    continue

  

  url = "https://www.parkrun.org.uk/" + name.replace(" ", "").lower() + "/results/eventhistory/"

  # Send a GET request to the URL
  response = requests.get(url, headers=headers)

  # Check if the request was successful
  if response.status_code == 200:
    # Parse the HTML content
    soup = BeautifulSoup(response.text, 'html.parser')

    # Find the table with the specified class
    table = soup.find('div', class_='Results Results--eventHistory')

    if table:
      with open(f"py/data/{name}_event_history.html", "w", encoding="utf-8") as file:
        file.write(table.prettify())
    else:
      print("Table with the specified class not found.")
  else:
    print(f"Failed to fetch {name}. Status code: {response.status_code}")

  if response.status_code == 405:
    exit()

    

  # Sleep for 30 seconds before fetching the next event
  print(f"{name}")
  time.sleep(30) # you can customise this if you want

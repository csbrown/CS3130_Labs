import requests
from bs4 import BeautifulSoup
import joblib
import functools
import itertools

RPUBS_URL = "https://rpubs.com/latest/{}"
def search(search_string, depth=100):
    search_string = search_string.lower()
    results = list(itertools.chain(
        joblib.Parallel(n_jobs=32)(joblib.delayed(
            functools.partial(search_page, search_string))(i) 
            for i in range(1, depth+1))
    ))
    return results

def search_page(search_string, i):
    print("searching page {}".format(i))
    results = []
    result = requests.get(RPUBS_URL.format(i))
    soup = BeautifulSoup(result.text, "html.parser")
    for pub in soup.find_all(class_="pubtile"):
        desc = pub.find(class_="desc").decode_contents().lower()
        link = pub.a["href"]
        title = pub.find_all("a")[1].decode_contents().lower()
        if (search_string in desc) or (search_string in title):
            results.append((title, desc, link))
            print(results[-1])
    return results

if __name__ == "__main__":
    import argparse
    import pprint
    parser = argparse.ArgumentParser()
    parser.add_argument("search_string")
    parser.add_argument("--depth", default=100, type=int)
    args = parser.parse_args()
    results = search(args.search_string, args.depth)
            

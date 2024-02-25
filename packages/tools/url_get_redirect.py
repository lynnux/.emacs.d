#!/usr/bin/env python
# -*- coding: utf-8 -*-

import requests


def get_redirect_url(url):
    response = requests.head(url, allow_redirects=False)
    if 302 == response.status_code:
        return response.headers["location"]
    else:
        return url
    
import os
import sys

def main():
    redirect_url = get_redirect_url(sys.argv[1])
    print(redirect_url)

if __name__ == "__main__":
    try:
        main()
    except:
        import traceback
        traceback.print_exc()
        input()

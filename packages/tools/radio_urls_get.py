#!/usr/bin/env python
# -*- coding: utf-8 -*-

import os
import sys

# 下载解压 https://backups.radio-browser.info/latest.sql.gz， 修改下面的为对应的文件路径

all_url = set()


def main():
    out = ""
    with open("H:\\download\\backup_de1_2024-02-22.sql", "r", encoding="utf-8") as f:
        a = f.read()
        cur = 0
        while cur >= 0:
            b = a.find("音乐", cur)
            e = a.find(")", b)
            cur = e
            # print(a[b-10: e])
            all = a[b - 20 : e].split(",")
            title = None
            url = None
            for s in all:
                if "音乐" in s:
                    title = s
                if title is not None and "http" in s:
                    url = s
                    break
            if url is None:
                continue
            if "listenurl" in url:
                continue
            # 蜻蜓音质都差得很，仅有一个音质好(硬编码到package_extra.el里了)
            if "lhttp.qingting.fm/" in url:
                continue
            if "lhttp.qtfm.cn/live" in url:
                continue
            # 去掉一些处理有问题的
            title = title.replace("'", '"')
            if title.count('"') != 2:
                continue
            # 去掉一些处理有问题的
            url = url.replace("'", '"')
            if url.count('"') != 2:
                continue
            # 去重复
            if url in all_url:
                continue            
            all_url.add(url)
            out += "(" + title + " " + url + ")" + "\n"
    with open("radio_urls.el", "w", encoding="utf-8") as f:
        f.write(out)
    input("over!")


if __name__ == "__main__":
    try:
        main()
    except:
        import traceback

        traceback.print_exc()
        input()

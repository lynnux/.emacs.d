#!/usr/bin/env python
# -*- coding: utf-8 -*-

# 目前只有解压zip包功能

import os
import sys
import zipfile
import shutil

# 第三个表示是否需要子目录，第四个表示解压前删除的目录
zip_list = [
    ("packages/auto-complete/auto-complete-master.zip", "packages/auto-complete", True),
    ("packages/company-mode/company-mode-master.zip", "packages/company-mode", True),
    ("packages/dired/dired-hacks-master.zip", "packages/dired", True),
    ("packages/easy-kill/easy-kill-extras.el-master.zip", "packages/easy-kill", True),
    ("packages/expand-region/expand-region.el-master.zip", "packages/expand-region", True),
    ("packages/helm/emacs-async-master.zip", "packages/helm"),
    ("packages/helm/helm-master.zip", "packages/helm"),
    ("packages/lsp/bui.el-master.zip", "packages/lsp"),
    ("packages/lsp/dap-mode-master.zip", "packages/lsp"),
    ("packages/lsp/eglot-master.zip", "packages/lsp", True),
    ("packages/lsp/lsp-mode-master.zip", "packages/lsp"),
    ("packages/lsp/lsp-pyright-master.zip", "packages/lsp"),
    ("packages/lsp/lsp-treemacs-master.zip", "packages/lsp"),
    ("packages/magit/magit-master.zip", "packages/magit"),
    ("packages/multiple-cursors/multiple-cursors.el-master.zip", "packages/multiple-cursors", True),
    ("packages/neotree/emacs-neotree-dev.zip", "packages/neotree", True),
    ("packages/projectile/rg.el-master.zip", "packages/projectile", True),
    ("packages/smartparens/smartparens-master.zip", "packages/smartparens", True),
    ("packages/swiper/swiper-master.zip", "packages/swiper", True),
    ("packages/treemacs/treemacs-master.zip", "packages/treemacs", True),
    ("packages/tree-sitter/elisp-tree-sitter-master.zip", "packages/tree-sitter", True),
    ("packages/tree-sitter/langs/tree-sitter-langs-master.zip", "packages/tree-sitter/langs", True),
    ("packages/tree-sitter/langs/elisp.zip", "packages/tree-sitter/langs", True),
    ("packages/use-package/use-package-master.zip", "packages/use-package", True),
    ("packages/yasnippet/yasnippet-snippets-master.zip", "packages/yasnippet"),
    ("themes/all-the-icons.el-master.zip", "themes"),
    ("themes/doom-modeline-master.zip", "themes"),
    ("themes/themes-master.zip", "themes"),
] 

def unzip(zf):
    path = ""
    out_dir = ""
    is_no_zip_root_dir = False
    dir_del_before_unzip = None
    if len(zf) == 2:
        (path, out_dir) = zf
    elif len(zf) == 3:
        (path, out_dir, is_no_zip_root_dir) = zf
    elif len(zf) == 3:
        (path, out_dir, is_no_zip_root_dir, dir_del_before_unzip) = zf

    if dir_del_before_unzip is not None:
        shutil.rmtree(dir_del_before_unzip, ignore_errors = True)
        
    zip_file = zipfile.ZipFile(path)
    zip_list = zip_file.namelist()
    for f in zip_list:
        zip_file.extract(f, out_dir) # 自己会覆盖的，这样后面更新包也没问题了
        
        if is_no_zip_root_dir:
            path1 = out_dir + "/" + f
            if os.path.isfile(path1): # 目录暂时不管了
                dest = out_dir + "/" + "/".join(f.split('/')[1:])

                if os.path.exists(dest):
                    os.unlink(dest)

                # print(path1, dest)
                if not os.path.exists(os.path.dirname(dest)):
                    os.makedirs(os.path.dirname(dest))
                    
                shutil.move(path1, dest) # 这个不能覆盖
                
    zip_file.close()

bin_list = [("https://ghproxy.com/https://github.com/BurntSushi/ripgrep/releases/download/13.0.0/ripgrep-13.0.0-x86_64-pc-windows-msvc.zip", "ripgrep-13.0.0-x86_64-pc-windows-msvc/rg.exe", "bin/rg.exe")]

import urllib.request
def download_bin():
    for bin in bin_list:
        (url, src, dest) = bin
        if os.path.exists(dest):
            continue
        print(url)
        dz = "bin/" + url.split('/')[-1]
        urllib.request.urlretrieve(url, dz)
        zip_file = zipfile.ZipFile(dz)
        zip_file.extract(src, dest)
        shutil.move(dest+"/"+src, dest)
        zip_file.close()
        
def unzip_el():
    print("el解压覆盖中...")
    if not os.path.exists("packages") or not os.path.exists("themes"):
        print("please run in ~/.emacs.d directory")
        return
    for f in zip_list:
        unzip(f)
        
def main():
    # download_bin()
    unzip_el()
    print('''
    操作完成，还有以下工作:
    1.安装字体：themes/all-the-icons.el-master/fonts\n
    2.bin/list.txt下载相关exe
    3.按packages/tree-sitter/说明.txt下载tree-sitter的langs对应的bin
    4.elisp的tree-sitter
    ''')
    os.system("pause")

if __name__ == "__main__":
    try:
        main()
    except:
        import traceback
        traceback.print_exc()
        input()

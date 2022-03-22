#!/usr/bin/env python
# -*- coding: utf-8 -*-

# 目前只有解压zip包功能

import os
import sys
import zipfile
import shutil
import urllib.request
import tarfile

# 第三个表示是否不要zip里的根目录，第四个表示解压前删除的目录
zip_list = [
    # http开头的排前面如果有问题就终止了
    ("https://ghproxy.com/https://github.com/emacs-tree-sitter/tree-sitter-langs/releases/download/0.11.3/tree-sitter-grammars.x86_64-pc-windows-msvc.v0.11.3.tar.gz", "packages/tree-sitter/langs/bin"),
    
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
    ("themes/nyan-mode-master.zip", "themes"),
] 
class ZipTar():
    def __init__(self, path):
        self.zip = None
        self.tar = None
        ext = path.split('.')[-1]
        if ext == 'zip':
            self.zip = zipfile.ZipFile(path)
        elif ext == 'tar':
            self.tar = tarfile.TarFile(path)
        elif ext == 'gz':
            self.tar = tarfile.open(path, mode="r:gz")

    def namelist(self):
        if self.zip is not None:
            return self.zip.namelist()
        elif self.tar is not None:
            return self.tar.getnames()
    def extract(self, member, path):
        if self.zip is not None:
            return self.zip.extract(member, path)
        elif self.tar is not None:
            return self.tar.extract(member, path)
    def close(self):
        if self.zip is not None:
            return self.zip.close()
        elif self.tar is not None:
            return self.tar.close()
        
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

    # 如果是网址就先下载
    if path.startswith("https://") or path.startswith("http://"):
        path = download_zip(path)
        
    if dir_del_before_unzip is not None:
        shutil.rmtree(dir_del_before_unzip, ignore_errors = True)
        
    zip_file = ZipTar(path)
    zip_list = zip_file.namelist()
    print(zip_list)
    for f in zip_list:
        zip_file.extract(f, out_dir) # 自己会覆盖的，这样后面更新包也没问题了
        
        if is_no_zip_root_dir:
            src = out_dir + "/" + f
            if os.path.isfile(src): # 目录暂时不管了
                dest = out_dir + "/" + "/".join(f.split('/')[1:])

                print(src, dest)
                
                # 因为shutil.move不能覆盖，故而要先删除
                if os.path.exists(dest):
                    os.unlink(dest)

                # 没有目录就创建目录
                if not os.path.exists(os.path.dirname(dest)):
                    os.makedirs(os.path.dirname(dest))
                    
                shutil.move(src, dest)
                
    zip_file.close()

bin_list = [
    ("http://adoxa.altervista.org/global/glo668wb.zip",[("bin/global.exe", "bin/global.exe"),
                                                        ("bin/gozilla.exe","bin/gozilla.exe"),
                                                        ("bin/gtags-cscope.exe","bin/gtags-cscope.exe"),
                                                        ("bin/gtags.exe","bin/gtags.exe"),
                                                        ("bin/htags.exe","bin/htags.exe"),
                                                        ]),
    ("https://ghproxy.com/https://github.com/BurntSushi/ripgrep/releases/download/13.0.0/ripgrep-13.0.0-x86_64-pc-windows-msvc.zip", [("ripgrep-13.0.0-x86_64-pc-windows-msvc/rg.exe", "bin/rg.exe")]),
    ("https://ghproxy.com/https://github.com/universal-ctags/ctags-win32/releases/download/2022-03-16%2Fp5.9.20220306.0-11-g4492555f/ctags-2022-03-16_p5.9.20220306.0-11-g4492555f-x64.zip", [
        ("ctags.exe", "bin/ctags.exe"),
        ("readtags.exe", "bin/readtags.exe")]),
    
    ("https://ghproxy.com/https://github.com/sharkdp/fd/releases/download/v8.3.2/fd-v8.3.2-x86_64-pc-windows-msvc.zip", [
        ("fd-v8.3.2-x86_64-pc-windows-msvc/fd.exe", "bin/fd.exe")
    ]),
    ("https://ghproxy.com/https://github.com/lynnux/.emacs.d/releases/download/20220319/my-emacs-bin_x64_msvc.zip",
     [("emacs-win32-launcher.exe", "bin/emacs-win32-launcher.exe"),
      ("pop_select.dll", "bin/pop_select.dll")])
]

def download_zip(url):
    print("downloading..." + url)
    # 下载到临时目录.cache里
    if not os.path.exists(".cache"):
        os.makedirs(".cache")
    dz = ".cache/" + url.split('/')[-1]
    urllib.request.urlretrieve(url, dz)
    return dz

def download_bin():
    for bin in bin_list:
        (url, filelist) = bin

        dz = download_zip(url)
        zip_file = ZipTar(dz)
        
        for (src, dest) in filelist:
            if os.path.exists(dest):
                os.unlink(dest)
                
            zip_file.extract(src, ".cache")
            shutil.move(".cache/"+src, dest)
            
        zip_file.close()
    
def unzip_el():
    print("el解压覆盖中...")
    if not os.path.exists("packages") or not os.path.exists("themes"):
        print("please run in ~/.emacs.d directory")
        return
    for f in zip_list:
        unzip(f)
        break
    
def main():
    download_bin()              # 先下载，网络失败的概率大
    unzip_el()
    print('''
    操作完成，还有以下工作:
    1.安装字体：themes/all-the-icons.el-master/fonts
    2.编译elc，打开init.el执行C-u 0 M-x byte-recompile-directory，
       完成后删除packages\tabbar\tabbar.elc，删除packages\easy-kill下除easy-kill.elc外的elc文件
    ''')
    os.system("pause")

if __name__ == "__main__":
    try:
        main()
    except:
        import traceback
        traceback.print_exc()
        input()

(setq byte-compile-warnings '(cl-functions)) ; disable "Package cl is deprecated"
(setq load-prefer-newer t) ; since 24.4 不加载过期elc文件
(setq native-comp-async-report-warnings-errors 'silent)

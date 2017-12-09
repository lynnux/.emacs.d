MajorVersion, MinorVersion, BuildNumber = utils.w32_version()
emacsclientw = ""
runemacs = ""

-- 
if "20100910-1853" == utils.system_name()
   and "Administrator" == utils.user_real_login_name()
   and MajorVersion == 5
   and MinorVersion == 1
   and BuildNumber == 2600
then
   emacsclientw = "D:\\green\\emacs-24.3\\bin\\emacsclientw.exe"
   runemacs = "D:\\green\\emacs-24.3\\bin\\runemacs.exe"
end

if "LYNNUX-PC" == utils.system_name()
   and "lynnux" == utils.user_real_login_name()
   and MajorVersion == 6
   and MinorVersion == 2
   and BuildNumber == 9200
then
   emacsclientw = "D:\\green\\emacs-25.3-x86_64\\bin\\emacsclientw.exe"
   runemacs = "D:\\green\\emacs-25.3-x86_64\\bin\\runemacs.exe"
end

-- work pc
if "XL-PC" == utils.system_name()
   and MajorVersion == 6
   and MinorVersion == 1
   and BuildNumber == 7601
then
   emacsclientw = "D:\\green\\emacs-w64-25.3-O2-with-modules\\emacs\\bin\\emacsclientw.exe"
   runemacs = "D:\\green\\emacs-w64-25.3-O2-with-modules\\emacs\\bin\\runemacs.exe"
end

-- get commandline
cmdline = utils.commandline()

-- check emacs is running
emacsIsRunning = false
emacsPid = 0
for k,v in Win32_Process.EnumAll() do
   if v == "emacs.exe" then
      emacsIsRunning = true
      emacsPid = k
      break
   end
end


if cmdline then
   if emacsIsRunning then
      utils.run_command(emacsclientw, "-n " .. cmdline)
   else
      utils.run_command(runemacs, cmdline)
   end
else
   if emacsIsRunning then
      -- utils.run_command(emacsclientw, "-t")
      utils.show_process(emacsPid)
   else
      utils.run_command(runemacs)
   end
end

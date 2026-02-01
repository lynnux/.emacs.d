-- emacs-win32-launcher --debug to see info
MajorVersion, MinorVersion, BuildNumber = utils.w32_version()
emacsclientw = ""
runemacs = ""

if "WIN-6PRFQIFSB6O" == utils.system_name()
--   and "lynnux" == utils.user_real_login_name()
   and MajorVersion == 6
   and MinorVersion == 1
   and BuildNumber == 7601
then
   emacsclientw = "C:\\green\\emacs\\gccemacs_240501\\bin\\emacsclientw.exe"
   runemacs = "C:\\green\\emacs\\gccemacs_240501\\bin\\runemacs.exe"
end

-- work pc
if "DESKTOP-N5D640Q" == utils.system_name()
   and MajorVersion == 6
   and MinorVersion == 2
   and BuildNumber == 9200
then
    emacsclientw = "D:\\green\\Emacs\\gccemacs_260201\\bin\\emacsclientw.exe"
    runemacs = "D:\\green\\Emacs\\gccemacs_260201\\bin\\runemacs.exe"
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

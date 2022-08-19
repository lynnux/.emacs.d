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
   emacsclientw = "C:\\green\\emacs\\gccemacs\\bin\\emacsclientw.exe"
   runemacs = "C:\\green\\emacs\\gccemacs\\bin\\runemacs.exe"
end

-- work pc
if "DESKTOP-9JD2LQI" == utils.system_name()
   and MajorVersion == 6
   and MinorVersion == 2
   and BuildNumber == 9200
then
   emacsclientw = "c:\\green\\Emacs\\emacs-28.1\\bin\\emacsclientw.exe"
   runemacs = "c:\\green\\Emacs\\emacs-28.1\\bin\\runemacs.exe"
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

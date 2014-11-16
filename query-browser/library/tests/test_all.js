
var fso = new ActiveXObject("Scripting.FileSystemObject");
var shell=  new ActiveXObject("WScript.Shell");

var args= ""
for(i= 0; i<WScript.Arguments.Count(); i++)
{
  args+= " " + WScript.Arguments(i)
}

if (WScript.FullName.indexOf("cscript.exe")==-1)
{
  WScript.Echo("This script may be runned only with CScript..  Try to run test_all.bat")  
  WScript.Quit(1)
}

var f= fso.GetFolder(".");
var fc= new Enumerator(f.SubFolders);

var common_res= 0
var line= ""

function output_stream(oExec)
{
  var out_eof= oExec.StdOut.AtEndOfStream
  while (!out_eof)
  {
    var output= oExec.StdOut.Read(1)
    line+= output
    if (line.indexOf("fail!")!=-1)
      common_res= 1
    if (output=="\n")
      line= ""
    WScript.StdOut.Write(output)
    out_eof= oExec.StdOut.AtEndOfStream
  }
  var err_eof= oExec.StdErr.AtEndOfStream
  while (!err_eof)
  {
    var output= oExec.StdErr.Read(1)
    WScript.StdOut.Write(output)
    err_eof= oExec.StdErr.AtEndOfStream
  }
}

for (; !fc.atEnd(); fc.moveNext())
{
  var dir_name= fc.item()

  if (!fso.FileExists(dir_name + "\\test.bat"))
    continue

  var old_directory= shell.CurrentDirectory
  shell.CurrentDirectory= dir_name

  var oExec= shell.Exec("test.bat " + args)

  while (oExec.Status == 0)
  {
    WScript.Sleep(100)
    output_stream(oExec)
  }
  output_stream(oExec)

  shell.CurrentDirectory= old_directory
}

if (!common_res)
{
  WScript.Echo("---------------------------------------------- mysql-query-browser tests pass!-")
  WScript.Quit(0)
}
else
{
  WScript.Echo("---------------------------------------------- mysql-query-browser tests fail!-")
  WScript.Quit(1)
}

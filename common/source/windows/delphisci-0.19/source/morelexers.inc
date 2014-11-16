{ Add more lexernames here, if there are more lexers in your version of 'SciLexer.Dll',
  i.e new version, custombuilt or other..

  All lexers which are added here is also selectable from within the IDE.

  If you wish to add those lexers at runtime, use AddLexer.
  That function checks whether a lexer already exists, and if not, adds it to the list.

  These will show up in the OptionsDialog (when you use Add language)
  when the dialog is called after they are added.

  NOTE: As the scintilla control treats the lexers casesensitive, this is also casesensitive.
  As new versions of the component is released, and new lexers are added in the main distro of
  the Scintilla project, the list of lexers will be updated in this include's parent file
  (ScintillaLanguageManager.pas). The reason for this includefile is to allow
  easy maintaining (for developers) of this file between versions..
}

{
  Add('blitzbasic');
}
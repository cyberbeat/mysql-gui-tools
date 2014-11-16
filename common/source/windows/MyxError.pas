unit MyxError;

// Copyright (C) 2003, 2004 MySQL AB
//
// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

interface

uses
  gnugettext, Classes, SysUtils, Forms, AuxFuncs,
  myx_util_public_interface;

type
  // Common base class for all MySQL exceptions.
  EMyxError = class(Exception)
  private
    FDescription: WideString;
  protected
    function GetFormattedMessage: WideString; virtual;
  public
    constructor Create(ADescription: WideString); virtual;

    property Description: WideString read FDescription;
    property FormattedMessage: WideString read GetFormattedMessage;
  end;

  EMyxEnhancedError = class(EMyxError)
  private
    FErrorNr: Integer;
    FErrorText: WideString;
  protected
    function GetFormattedMessage: WideString; override;
  public
    constructor Create(ADescription: WideString; AErrorNr: Integer; AErrorText: WideString); reintroduce; virtual;

    property ErrorNr: Integer read FErrorNr;
    property ErrorText: WideString read FErrorText;
  end;

  EMyxCriticalError = class(EMyxEnhancedError)
  public
    constructor Create(ADescription: WideString; ErrorNr: Integer); reintroduce; virtual;
  end;

  EMyxSystemError = class(EMyxEnhancedError)
  protected
    function GetFormattedMessage: WideString; override;
  public
    constructor Create(ADescription: WideString; ErrorNr: Integer); reintroduce; virtual;
  end;

  EMyxSQLError = class(EMyxEnhancedError)
  protected
    function GetFormattedMessage: WideString; override;
  end;

  EMyxMultiSQLError = class(EMyxEnhancedError)
  protected
    function GetFormattedMessage: WideString; override;
  public
    constructor Create(ADescription, AMessage: WideString); reintroduce; virtual;
  end;

  EMyxLibraryError = class(EMyxEnhancedError)
  private
    FErrorNrParam: WideString;
  protected
    function GetFormattedMessage: WideString; override;
  public
    constructor Create(ADescription: WideString; AErrorNr: Integer; AErrorNrParam: WideString); reintroduce; virtual;

    property ErrorNrParam: WideString read FErrorNrParam;
  end;

  EMyxCommonLibraryError = class(EMyxLibraryError)
  public
    constructor Create(ADescription: WideString; AErrorNr: Integer; AErrorNrParam: WideString); override;
  end;

procedure ShowError(E: Exception);

//----------------------------------------------------------------------------------------------------------------------

implementation

//----------------- EMyxError- -----------------------------------------------------------------------------------------

constructor EMyxError.Create(ADescription: WideString);

begin
  FDescription := ADescription;

  // Set the base exception description as well, although due to the ANSI conversion some info might be lost.
  Message := ADescription;
end;

//----------------------------------------------------------------------------------------------------------------------

function EMyxError.GetFormattedMessage: WideString;

// Returns a composed message about the exception. Descendants should override for more meaningfull output.

begin
  Result := FDescription;
end;

//----------------- EMyxEnhancedError ----------------------------------------------------------------------------------

constructor EMyxEnhancedError.Create(ADescription: WideString; AErrorNr: Integer; AErrorText: WideString);

begin
  inherited Create(ADescription);
  FErrorNr := AErrorNr;
  FErrorText := AErrorText;
end;

//----------------------------------------------------------------------------------------------------------------------

function EMyxEnhancedError.GetFormattedMessage: WideString;

begin
  Result := WideFormat(_('%s'#10' The following error occured: %s (%d)'), [inherited GetFormattedMessage, FErrorText, FErrorNr]);
end;

//----------------- EMyxCriticalError ----------------------------------------------------------------------------------

constructor EMyxCriticalError.Create(ADescription: WideString; ErrorNr: Integer);

begin
  inherited Create(ADescription, ErrorNr, '');
end;

//----------------- EMyxSystemError ------------------------------------------------------------------------------------

constructor EMyxSystemError.Create(ADescription: WideString; ErrorNr: Integer);

begin
  inherited Create(ADescription, ErrorNr, SysErrorMessage(ErrorNr));
end;

//----------------------------------------------------------------------------------------------------------------------

function EMyxSystemError.GetFormattedMessage: WideString;

begin
  Result := WideFormat(_('System Error Number %d'#10'%s'), [ErrorNr, ErrorText]);
end;

//----------------- EMyxSQLError ---------------------------------------------------------------------------------------

function EMyxSQLError.GetFormattedMessage: WideString;

begin
  Result := WideFormat(_('A MySQL error was encountered. The message is:' + #13#10 + '%s'), [inherited GetFormattedMessage]);
end;

//----------------- EMyxMultiSQLError ----------------------------------------------------------------------------------

constructor EMyxMultiSQLError.Create(ADescription, AMessage: WideString);

begin
  inherited Create(ADescription, 0, AMessage);
end;

//----------------------------------------------------------------------------------------------------------------------

function EMyxMultiSQLError.GetFormattedMessage: WideString;

begin
  Result := WideFormat(_('There was a problem: %s') + #13#10 + _('Following warnings / errors have been encountered:') +
    #13#10 + '%s', [FDescription, FErrorText]);
end;

//----------------- EMyxLibraryError -----------------------------------------------------------------------------------

constructor EMyxLibraryError.Create(ADescription: WideString; AErrorNr: Integer; AErrorNrParam: WideString);

begin
  inherited Create(ADescription, AErrorNr, AErrorNrParam);
end;

//----------------------------------------------------------------------------------------------------------------------

function EMyxLibraryError.GetFormattedMessage: WideString;

begin
  Result := WideFormat(_('Library Error Number %d' + #13#10 + '%s'), [ErrorNr, ErrorNrParam]);
end;

//----------------- EMyxCommonLibraryError -----------------------------------------------------------------------------

constructor EMyxCommonLibraryError.Create(ADescription: WideString; AErrorNr: Integer; AErrorNrParam: WideString);

begin
  case MYX_LIB_ERROR(FErrorNr) of
    MYX_ERROR_CANT_OPEN_FILE:
      ADescription := WideFormat(_('The File %s cannot be opened.'), [ErrorNrParam]);
    MYX_ERROR_CANT_CONNECT_TO_INSTANCE:
      ADescription := _('A connection to the server cannot be established.');
    MYX_XML_PARSE_ERROR:
      ADescription := WideFormat(_('An error occured while parsing the XML file %s.'), [ErrorNrParam]);
    MYX_XML_NO_VALID_DOCUMENT:
      ADescription := WideFormat(_('An error occured while validating the XML file %s.'), [ErrorNrParam]);
    MYX_XML_EMPTY_DOCUMENT:
      ADescription := WideFormat(_('The XML file %s is empty.'), [ErrorNrParam]);
    MYX_SQL_ERROR:
      ADescription := _('An SQL error occured.');
    MYX_STOP_EXECUTION:
      ADescription := _('The execution was stopped.');
  end;

  inherited;
end;

//----------------------------------------------------------------------------------------------------------------------

procedure ShowError(E: Exception);

var
  res: Integer;
  MyxError: EMyxError;
  Message: WideString;

begin
  if not (Application.Terminated) then
  begin
    if E is EMyxError then
    begin
      MyxError := E as EmyxError;
      Message := MyxError.FormattedMessage;

      if (E is EMyxCriticalError) then
      begin
        res := ShowModalDialog(Application.Title + ' ' + _('Critical Error'),
          Format(_('The following Exception occurred:' + #13#10 + '%s' + #13#10#13#10 +
          'Please choose whether to continue %s, terminate the application ' +
          'or restart the program.'), [Message, Application.Title]),
          myx_mtError, _('Continue') + #13#10 + _('Terminate') + #13#10 + _('Restart'));

        if (res = 2) then
          Application.Terminate
        else
          if (res = 3) then
          begin
            CreateSubProcess(Application.ExeName);
            Application.Terminate;
          end;
      end
      else
        if (E is EMyxSQLError) then
        begin
          ShowModalDialog(Application.Title + ' ' + _('SQL Error'), Message , myx_mtError, _('OK'));
        end
        else
          if (E is EMyxSystemError) then
          begin
            ShowModalDialog(Application.Title + ' ' + _('System Error'), Message, myx_mtError, _('OK'));
          end
          else
            if (E is EMyxLibraryError) then
            begin
              ShowModalDialog(Application.Title + ' ' + _('Library Error'), Message, myx_mtError, _('OK'));
            end
            else
            begin
              ShowModalDialog(Application.Title + ' ' + _('Error'), _('The following exception occurred:') + #13#10#13#10 +
                Message, myx_mtError, _('OK'));
            end
    end;
    end
    else
    begin
      ShowModalDialog(Application.Title + ' ' + _('Error'), _('The following exception occurred:') + #13#10#13#10 +
        E.Message, myx_mtError, 'OK');
    end;
end;

//----------------------------------------------------------------------------------------------------------------------

end.


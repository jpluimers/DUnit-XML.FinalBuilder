///  This is a modified XMLTestRunner which outputs in NUnit format
///  so that it can be used with FinalBuilder.

(*
 * The contents of this file are subject to the Mozilla Public
 * License Version 1.1 (the "License"); you may not use this file
 * except in compliance with the License. You may obtain a copy of
 * the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS
 * IS" basis, WITHOUT WARRANTY OF ANY KIND, either express or
 * implied. See the License for the specific language governing
 * rights and limitations under the License.
 *
 * The Original Code is DUnit.
 *
 * The Initial Developers of the Original Code are Kent Beck, Erich Gamma,
 * and Juancarlo Añez.
 * Portions created The Initial Developers are Copyright (C) 1999-2000.
 * Portions created by The DUnit Group are Copyright (C) 2000-2003.
 * All rights reserved.
 *
 * Contributor(s):
 * Kent Beck <kentbeck@csi.com>
 * Erich Gamma <Erich_Gamma@oti.com>
 * Juanco Añez <juanco@users.sourceforge.net>
 * Chris Morris <chrismo@users.sourceforge.net>
 * Jeff Moore <JeffMoore@users.sourceforge.net>
 * Kris Golko <neuromancer@users.sourceforge.net>
 * The DUnit group at SourceForge <http://dunit.sourceforge.net>
 *
 *)

{
 Contributor : Laurent Laffont <llaffont@altaiire.fr>
}

{$if RTLVersion >= 21.0}
  {$define HasStopwatch} // Delphi 2010 and up has a cross platform TStopwatch.
  // http://docs.embarcadero.com/products/rad_studio/delphiAndcpp2009/HelpUpdate2/EN/html/delphivclwin32/contents.html
  // http://docwiki.embarcadero.com/VCL/2010/en/Diagnostics.TStopwatch
{$ifend}

unit FinalBuilder.XMLTestRunner;

interface
uses
  SysUtils,
  Classes,
{$ifdef HasStopwatch}
  Diagnostics,
{$endif HasStopwatch}
  TestFramework;

const
   DEFAULT_FILENAME = 'dunit-report.xml';

type
{$ifndef HasStopwatch}
  TStopwatch = record // Windows dependent (because of high resolution timer) TStopwatch
  strict private
    class var FFrequency: Int64;
    class var FIsHighResolution: Boolean;
    class var TickFrequency: Double;
    FElapsed: Int64;
    FRunning: Boolean;
    FStartTimeStamp: Int64;
    function GetElapsedDateTimeTicks: Int64;
    function GetElapsedMilliseconds: Int64;
    function GetElapsedTicks: Int64;
    class procedure InitStopwatchType; static;
  public
    class function GetTimeStamp: Int64; static;
    class function StartNew: TStopwatch; static;
    procedure Stop;
    procedure Reset;
    procedure Start;
    property ElapsedTicks: Int64 read GetElapsedTicks;
    property ElapsedMilliseconds: Int64 read GetElapsedMilliseconds;
  end;
{$endif HasStopwatch}

  TXMLTestListener = class(TInterfacedObject, ITestListener, ITestListenerX)
  private
    FOutputFile : TFileStream;
    FFileName : String;

  protected
    stopWatch : TStopWatch;
    dtStartTime : TDateTime;
    dtEndTime : TDateTime;

    testStart : TDateTime;
    FSuiteStack : TStringList;
    FInfoList   : TStringList;
    FWarningList : TStringList;

    procedure writeReport(str: String);

    function GetCurrentSuiteName : string;
    function  PrintErrors(r: TTestResult): string; virtual;
    function  PrintFailures(r: TTestResult): string; virtual;
    function  PrintHeader(r: TTestResult): string; virtual;
    function  PrintFailureItems(r :TTestResult): string; virtual;
    function  PrintErrorItems(r :TTestResult): string; virtual;
    function  Report(r: TTestResult): string;

  public
    function HasInfoOrWarnings : boolean;
    procedure WriteInfoAndWarnings;

    // implement the ITestListener interface
    procedure AddSuccess(test: ITest); virtual;
    procedure AddError(error: TTestFailure); virtual;
    procedure AddFailure(failure: TTestFailure); virtual;
    function  ShouldRunTest(test :ITest):boolean; virtual;
    procedure StartSuite(suite: ITest); virtual;
    procedure EndSuite(suite: ITest); virtual;
    procedure StartTest(test: ITest); virtual;
    procedure EndTest(test: ITest); virtual;
    procedure TestingStarts; virtual;
    procedure TestingEnds(testResult: TTestResult); virtual;
    procedure Status(test :ITest; const Msg :string);
    procedure Warning(test :ITest; const Msg :string);

    constructor Create; overload;
    constructor Create(outputFile : String); overload;
    destructor Destroy; override;

    class function RunTest(suite: ITest; outputFile:String): TTestResult; overload;
    class function RunRegisteredTests(outputFile:String): TTestResult;
    class function text2sgml(text : String) : String;
    class function StringReplaceAll (text,byt,mot : string ) :string;

    //:Report filename. If an empty string, then standard output is used (compile with -CC option)
    property FileName : String read FFileName write FFileName;
  end;

{: Run the given test suite
}
function RunTest(suite: ITest; outputFile:String=DEFAULT_FILENAME) : TTestResult; overload;
function RunRegisteredTests(outputFile:String=DEFAULT_FILENAME) : TTestResult; overload;

var
  PrintReportToConsole : boolean = true;

implementation

{$ifndef HasStopwatch}
uses
  Windows;

const
  TicksPerMillisecond = 10000;
  TicksPerSecond = 1000 * Int64(TicksPerMillisecond);

function TStopwatch.GetElapsedDateTimeTicks: Int64;
begin
  Result := ElapsedTicks;
  if FIsHighResolution then
    Result := Trunc(Result * TickFrequency);
end;

function TStopwatch.GetElapsedMilliseconds: Int64;
begin
  Result := GetElapsedDateTimeTicks div TicksPerMillisecond;
end;

function TStopwatch.GetElapsedTicks: Int64;
begin
  Result := FElapsed;
  if FRunning then
    Result := Result + GetTimeStamp - FStartTimeStamp;
end;

class function TStopwatch.GetTimeStamp: Int64;
begin
  if FIsHighResolution then
    QueryPerformanceCounter(Result)
  else
    Result := GetTickCount * TicksPerMillisecond;
end;

class procedure TStopwatch.InitStopwatchType;
begin
  if FFrequency = 0 then
  begin
    if not QueryPerformanceFrequency(FFrequency) then
    begin
      FIsHighResolution := False;
      FFrequency := TicksPerSecond;
      TickFrequency := 1.0;
    end else
    begin
      FIsHighResolution := True;
      TickFrequency := 10000000.0 / FFrequency;
    end;
  end;
end;

procedure TStopwatch.Reset;
begin
  FElapsed := 0;
  FRunning := False;
  FStartTimeStamp := 0;
end;

procedure TStopwatch.Start;
begin
  if not FRunning then
  begin
    FStartTimeStamp := GetTimeStamp;
    FRunning := True;
  end;
end;

class function TStopwatch.StartNew: TStopwatch;
begin
  InitStopwatchType;
  Result.Reset;
  Result.Start;
end;

procedure TStopwatch.Stop;
begin
  if FRunning then
  begin
    FElapsed := FElapsed + GetTimeStamp - FStartTimeStamp;
    FRunning := False;
  end;
end;
{$endif HasStopwatch}

const
   CRLF = #13#10;
   MAX_DEEP = 5;

function ExeName: string;
begin
  Result := ParamStr(0);
end;

function IsValidXMLChar(wc: WideChar): Boolean;
begin
  case Word(wc) of
    $0009, $000A, $000C, $000D,
      $0020..$D7FF,
      $E000..$FFFD, // Standard Unicode chars below $FFFF
      $D800..$DBFF, // High surrogate of Unicode character  = $10000 - $10FFFF
      $DC00..$DFFF: // Low surrogate of Unicode character  = $10000 - $10FFFF
      result := True;
  else
    result := False;
  end;
end;


function StripInvalidXML(const s: string): string;
var
  i, count: Integer;
begin
  count := Length(s);
  setLength(result, count);
  for i := 1 to Count do // Iterate
  begin
    if IsValidXMLChar(WideChar(s[i])) then
      result[i] := s[i]
    else
      result[i] := ' ';
  end; // for}
end;


function EscapeForXML(const value: string; const isAttribute: boolean = True; const isCDATASection : Boolean = False): string;
begin
  result := StripInvalidXML(value);
  if isCDATASection  then
  begin
    Result := StringReplace(Result, ']]>', ']>',[rfReplaceAll]);
    exit;
  end;

  //note we are avoiding replacing &amp; with &amp;amp; !!
  Result := StringReplace(result, '&amp;', '[[-xy-amp--]]',[rfReplaceAll]);
  Result := StringReplace(result, '&', '&amp;',[rfReplaceAll]);
  Result := StringReplace(result, '[[-xy-amp--]]', '&amp;amp;',[rfReplaceAll]);
  Result := StringReplace(result, '<', '&lt;',[rfReplaceAll]);
  Result := StringReplace(result, '>', '&gt;',[rfReplaceAll]);

  if isAttribute then
  begin
    Result := StringReplace(result, '''', '&#39;',[rfReplaceAll]);
    Result := StringReplace(result, '"', '&quot;',[rfReplaceAll]);
  end;
end;


{ TXMLTestListener }
   
constructor TXMLTestListener.Create;
begin
   Create(DEFAULT_FILENAME);
end;

constructor TXMLTestListener.Create(outputFile : String);
begin
   inherited Create;
   FileName     := outputFile;
   FSuiteStack  := TStringList.Create;
   FInfoList    := TStringList.Create;
   FWarningList := TStringList.Create;
end;


procedure TXMLTestListener.writeReport(str : String);
{$IFDEF UNICODE}
var
  Buffer : TBytes;
{$ENDIF}
begin
  str := str + CRLF;
  {$IFDEF UNICODE}
  if FOutputFile <> nil then
  begin
    buffer := TEncoding.UTF8.GetBytes(str);
    FOutputFile.WriteBuffer(buffer[0],Length(buffer));
  end
  else
    WriteLn(str);
  {$ELSE}
  if FOutputFile <> nil then
    FOutputFile.Write(PChar(str)^,Length(str))
  else
    Writeln(str);
  {$ENDIF}
end;

const
  TrueFalse : array[Boolean] of string = ('False', 'True');

procedure TXMLTestListener.AddSuccess(test: ITest);
var
  endTag : string;
begin
   if test.tests.Count<=0 then
   begin
      if HasInfoOrWarnings then
        endTag := '>'
      else
        endTag := '/>';

      writeReport(Format('<test-case name="%s%s" executed="%s" success="True" time="%1.3f" result="Pass" %s',
                         [EscapeForXML(GetCurrentSuiteName), EscapeForXML(test.GetName), TrueFalse[test.Enabled], test.ElapsedTestTime / 1000,endTag]));
      if HasInfoOrWarnings then
      begin
        WriteInfoAndWarnings;
        writeReport('</test-case>');
      end;

   end;
end;

procedure TXMLTestListener.AddError(error: TTestFailure);
begin
   writeReport(Format('<test-case name="%s%s" executed="%s" success="False" time="%1.3f" result="Error">',
                      [EscapeForXML(GetCurrentSuiteName), EscapeForXML(error.FailedTest.GetName), TrueFalse[error.FailedTest.Enabled], error.FailedTest.ElapsedTestTime / 1000]));
   writeReport(Format('<failure name="%s" location="%s">', [EscapeForXML(error.ThrownExceptionName), EscapeForXML(error.LocationInfo)]));
   writeReport('<message>' + EscapeForXML(error.ThrownExceptionMessage,false) + '</message>');
   writeReport('</failure>');
   WriteInfoAndWarnings;
   writeReport('</test-case>');
end;

procedure TXMLTestListener.AddFailure(failure: TTestFailure);
begin
   writeReport(Format('<test-case name="%s%s" executed="%s" success="False" time="%1.3f" result="Failure">',
                      [EscapeForXML(GetCurrentSuiteName), EscapeForXML(failure.FailedTest.GetName), TrueFalse[failure.FailedTest.Enabled], failure.FailedTest.ElapsedTestTime / 1000]));
   writeReport(Format('<failure name="%s" location="%s">', [EscapeForXML(failure.ThrownExceptionName), EscapeForXML(failure.LocationInfo)]));
   writeReport('<message>' + EscapeForXML(failure.ThrownExceptionMessage,false) + '</message>');
   writeReport('</failure>');
   WriteInfoAndWarnings;
   writeReport('</test-case>');
end;


procedure TXMLTestListener.StartTest(test: ITest);
begin
  FInfoList.Clear;
  FWarningList.Clear;
end;

procedure TXMLTestListener.EndTest(test: ITest);
begin
end;

procedure TXMLTestListener.TestingStarts;
{$IFDEF UNICODE}
var
  Preamble: TBytes;
{$ENDIF}
begin
   stopWatch := TStopWatch.StartNew();
   dtStartTime := Now;
   ForceDirectories(ExtractFilePath(FFileName));
   FOutputFile := TFileStream.Create(FFileName,fmCreate);
{$IFDEF UNICODE}
   //write the byte order mark
   Preamble := TEncoding.UTF8.GetPreamble;
   if Length(Preamble) > 0 then
      FOutputFile.WriteBuffer(Preamble[0], Length(Preamble));
   writeReport('<?xml version="1.0" encoding="UTF-8" standalone="yes" ?>');
{$ELSE}
   writeReport('<?xml version="1.0" encoding="ISO-8859-1" standalone="yes" ?>');
{$ENDIF}
   writeReport(Format('<test-results total="%d" notrun="%d" date="%s" time="%s" >',
                      [RegisteredTests.CountTestCases,
                        RegisteredTests.CountTestCases - RegisteredTests.CountEnabledTestCases,
                          DateToStr(Now),
                            TimeToStr(Now)]));
   writeReport(Format('<application name="%s" />',[ExtractFileName(ParamStr(0))]));
end;

procedure TXMLTestListener.TestingEnds(testResult: TTestResult);
var
  runTime : Double;
  dtRunTime : TDateTime;
  successRate : Integer;
  h, m, s, l :Word;
begin
   stopWatch.Stop();
   runtime := stopWatch.ElapsedMilliseconds / 1000;

   if testResult.RunCount > 0 then
     successRate :=  Trunc(
        ((testResult.runCount - testResult.failureCount - testResult.errorCount)
         /testResult.runCount)
        *100)
   else
     successRate := 100;

   writeReport('<statistics>'+CRLF+
                  '<stat name="tests" value="'+intToStr(testResult.runCount)+'" />'+CRLF+
                  '<stat name="failures" value="'+intToStr(testResult.failureCount)+'" />'+CRLF+
                  '<stat name="errors" value="'+intToStr(testResult.errorCount)+'" />'+CRLF+
                  '<stat name="success-rate" value="'+intToStr(successRate)+'%" />'+CRLF+
                  '<stat name="started-at" value="'+DateTimeToStr(dtStartTime)+'" />'+CRLF+
                  '<stat name="finished-at" value="'+DateTimeToStr(now)+'" />'+CRLF+
                  Format('<stat name="runtime" value="%1.3f"/>', [runtime])+CRLF+
                  '</statistics>'+CRLF+
              '</test-results>');
   FreeAndNil(FOutputFile);

   if PrintReportToConsole then
   begin
      dtEndTime := now;
      dtRunTime := Now-dtStartTime;
      writeln;
      DecodeTime(dtRunTime, h,  m, s, l);
      writeln(Format('Time: %d:%2.2d:%2.2d.%d', [h, m, s, l]));
      writeln(Report(testResult));
      writeln;

   end;
end;

class function TXMLTestListener.RunTest(suite: ITest; outputFile:String): TTestResult;
begin
   Result := TestFramework.RunTest(suite, [TXMLTestListener.Create(outputFile)]);
end;

function TXMLTestListener.Report(r: TTestResult): string;
begin
  result := PrintHeader(r) +
            PrintErrors(r) +
            PrintFailures(r);
end;

class function TXMLTestListener.RunRegisteredTests(outputFile:String): TTestResult;
begin
  Result := RunTest(registeredTests, outputFile);
end;

function RunTest(suite: ITest; outputFile:String=DEFAULT_FILENAME): TTestResult;
begin
   Result := TestFramework.RunTest(suite, [TXMLTestListener.Create(outputFile)]);
end;

function RunRegisteredTests(outputFile:String=DEFAULT_FILENAME): TTestResult;
begin
   Result := RunTest(registeredTests, outputFile);
end;


procedure TXMLTestListener.Status(test: ITest; const Msg: string);
begin
  FInfoList.Add(Format('STATUS: %s: %s', [test.Name, Msg]));
end;

procedure TXMLTestListener.Warning(test :ITest; const Msg :string);
begin
  FWarningList.Add(Format('WARNING: %s: %s', [test.Name, Msg]));
end;

procedure TXMLTestListener.WriteInfoAndWarnings;
var
  i: Integer;
begin
  if FInfoList.Count > 0 then
  begin
    for i := 0 to FInfoList.Count - 1 do
        writeReport('<status>' + EscapeForXML(FInfoList.Strings[i],false) + '</status>');
  end;
  if FWarningList.Count > 0 then
  begin
    for i := 0 to FInfoList.Count - 1 do
        writeReport('<warning>' + EscapeForXML(FWarningList.Strings[i],false) + '</warning>');
  end;
end;

function TXMLTestListener.ShouldRunTest(test: ITest): boolean;
begin
  Result := test.Enabled;
  if not Result then
    writeReport(Format('<test-case name="%s%s" executed="False"/>',
                       [GetCurrentSuiteName, test.GetName]));
end;

procedure TXMLTestListener.EndSuite(suite: ITest);
begin
     if CompareText(suite.Name, ExtractFileName(ExeName)) = 0 then
       Exit;
     writeReport('</results>');
     writeReport('</test-suite>');
     FSuiteStack.Delete(0);
end;

procedure TXMLTestListener.StartSuite(suite: ITest);
var
  s : string;
begin
   if CompareText(suite.Name, ExtractFileName(ExeName)) = 0 then
     Exit;
   s := GetCurrentSuiteName + suite.Name;
   writeReport(Format('<test-suite name="%s" total="%d" notrun="%d">', [s, suite.CountTestCases, suite.CountTestCases - suite.CountEnabledTestCases]));
   FSuiteStack.Insert(0, suite.getName);
   writeReport('<results>');
end;

{:
 Replace byt string by mot in text string
 }
class function TXMLTestListener.StringReplaceAll (text,byt,mot : string ) :string;
var
   plats : integer;
begin
While pos(byt,text) > 0 do
      begin
      plats := pos(byt,text);
      delete (text,plats,length(byt));
      insert (mot,text,plats);
      end;
result := text;
end;

{:
 Replace special character by sgml compliant characters
 }
class function TXMLTestListener.text2sgml(text : String) : String;
begin
  text := stringreplaceall (text,'<','&lt;');
  text := stringreplaceall (text,'>','&gt;');
  result := text;
end;

destructor TXMLTestListener.Destroy;
begin
  FreeAndNil(FSuiteStack);
  FreeAndNil(FOutputFile);
  FreeAndNil(FInfoList);
  FreeAndNil(FWarningList);
  inherited Destroy;
end;

function TXMLTestListener.GetCurrentSuiteName: string;
var
  c : Integer;
begin
  Result := '';
  for c := 0 to FSuiteStack.Count - 1 do
    Result := FSuiteStack[c] + '.' + Result;
end;

function TXMLTestListener.HasInfoOrWarnings: boolean;
begin
  result := (FInfoList.Count > 0) or (FWarningList.Count > 0);
end;

function TXMLTestListener.PrintErrorItems(r: TTestResult): string;
var
  i: Integer;
  failure: TTestFailure;
begin
  result := '';
  for i := 0 to r.ErrorCount-1 do begin
    failure := r.Errors[i];
    result := result + format('%3d) %s: %s'#13#10'     at %s'#13#10'      "%s"',
                               [
                               i+1,
                               failure.failedTest.name,
                               failure.thrownExceptionName,
                               failure.LocationInfo,
                               failure.thrownExceptionMessage
                               ]) + CRLF;
  end;
end;

function TXMLTestListener.PrintErrors(r: TTestResult): string;
begin
  result := '';
  if (r.errorCount <> 0) then begin
    if (r.errorCount = 1) then
      result := result + format('There was %d error:', [r.errorCount]) + CRLF
    else
      result := result + format('There were %d errors:', [r.errorCount]) + CRLF;

    result := result + PrintErrorItems(r);
    result := result + CRLF
  end
end;

function TXMLTestListener.PrintFailureItems(r: TTestResult): string;
var
  i: Integer;
  failure: TTestFailure;
begin
  result := '';
  for i := 0 to r.FailureCount-1 do begin
    failure := r.Failures[i];
    result := result + format('%3d) %s: %s'#13#10'     at %s'#13#10'      "%s"',
                               [
                               i+1,
                               failure.failedTest.name,
                               failure.thrownExceptionName,
                               failure.LocationInfo,
                               failure.thrownExceptionMessage
                               ]) + CRLF;
  end;
end;

function TXMLTestListener.PrintFailures(r: TTestResult): string;
begin
  result := '';
  if (r.failureCount <> 0) then begin
    if (r.failureCount = 1) then
      result := result + format('There was %d failure:', [r.failureCount]) + CRLF
    else
      result := result + format('There were %d failures:', [r.failureCount]) + CRLF;

    result := result + PrintFailureItems(r);
    result := result + CRLF
  end
end;

function TXMLTestListener.PrintHeader(r: TTestResult): string;
begin
  result := '';
  if r.wasSuccessful then
  begin
    result := result + CRLF;
    result := result + format('OK: %d tests'+CRLF, [r.runCount]);
  end
  else
  begin
    result := result + CRLF;
    result := result + 'FAILURES!!!'+CRLF;
    result := result + 'Test Results:'+CRLF;
    result := result + format('Run:      %8d'+CRLF+'Failures: %8d'+CRLF+'Errors:   %8d'+CRLF,
                      [r.runCount, r.failureCount, r.errorCount]
                      );
  end
end;

end.

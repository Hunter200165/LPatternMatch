unit LPatternMatch.Core;

(*
	LPatternMatch.Core - simple to use pattern matching engine, which was adapted from
	Lua 5.3 source code (all license notices are presented below).

	Written/Adapted by Hunter200165, 2021;

	Unit might be used under the terms of MIT License.
*)

(*
	Copyright © 1994–2021 Lua.org, PUC-Rio.
	Permission is hereby granted, free of charge, to any person obtaining a copy
	of this software and associated documentation files (the "Software"), to deal
	in the Software without restriction, including without limitation the rights
	to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
	copies of the Software, and to permit persons to whom the Software is furnished
	to do so, subject to the following conditions:

	The above copyright notice and this permission notice shall be included in all
	copies or substantial portions of the Software.

	THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED,
	INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
	PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
	HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION
	OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE
	OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
*)

{$IfDef FPC}
	{$mode delphi}
	{$H+}
{$EndIf}

interface

{$Define Unicode}
{$IfNDef Unicode}
	{.$Define UseStaticRanges}
{$Else}
	{$UnDef UseStaticRanges}
{$EndIf}

type
{$IfDef Unicode}
	TLPMString = UnicodeString;
	TLPMChar = UnicodeChar;
{$Else}
	TLPMString = AnsiString;
	TLPMChar = Char;
{$EndIf}
	PLPMChar = ^TLPMChar;

const LPM_CAP_UNFINISHED = -1;
const LPM_CAP_POSITION = -2;

const LPM_MAX_CAPTURES = 64;

const LPM_L_ESC = '%';
{ Better - we include L_ESC in special set directly }
const LPM_SPECIALS = [ '^', '$', '*', '+', '?', '.', '(', '[', '-', LPM_L_ESC ];

{ Max depth of recursion in call to Match method }
const LPM_MAX_CALLS = 256;

type 
	TLPMResult = Int32;

const
	{ Defines normal result }
	LPM_RESULT_OK = 0;
	{ Errors that might happen in Match function }
	LPM_RESULT_PATTERN_IS_TOO_COMPLEX = 10;
	LPM_RESULT_PATTERN_EXPECTED_SQUARE_BRACKET_AFTER_FRONTIER = 11;
	{ Errors that might happen in StartCaputre function }
	LPM_RESULT_TOO_MANY_CAPTURES = 20;
	{ Errors that might happen in CheckCapture function }
	LPM_RESULT_INVALID_CAPTURE_INDEX = 30;
	{ Errors that might happen in CaptureToClose function }
	LPM_RESULT_INVALID_PATTERN_CAPTURE = 40;
	{ Errors that might happen in MatchBalance function }
	LPM_RESULT_MISSING_ARGUMENTS_FOR_BALANCE = 50;
	{ Errors that might happen in ClassEnd function }
	LPM_RESULT_CLASS_END_ENDS_WITH_ESCAPE_CHAR = 60;
	LPM_RESULT_CLASS_DOES_NOT_END_WITH_SQUARE_BRACKET = 61;

type
	{ Record representing one particular capture }
	TLPMCapture = packed record
		Init: PLPMChar;
		{ Added position }
		Position: Int32;
		Length: Int32;
	end;
	{ Static array of  }
	TLPMCaptures = array [0..(LPM_MAX_CAPTURES - 1)] of TLPMCapture;

type
	{ TLPMMatchState }
	TLPMMatchState = packed record
	public var
		{ Begin of source string }
		SrcInit: PLPMChar;
		{ End of source string (usually \0 character, but may be any) }
		SrcEnd: PLPMChar;
		{ End of pattern string (usually \0 character, but may be any) }
		PEnd: PLPMChar;
	private var
		{ Shows recursion depth of match function }
		MatchDepth: Int32;
	public var
		{ Captures count }
		Level: Int32;
		{ Captures themselves }
		Captures: TLPMCaptures;
	public
		{ Does pattern contain special symbols? }
		class function NoSpecials(P: PLPMChar; L: Int32): Boolean; static;
		{ Find position of S2 in S1 and return the entry (pointer) or nil, if nothing is found.
			The reason that it exists here - it is that Pos function does not work with pointers (PChars), so I implemented one that was in lstrlib.c
		}
		class function LMemFind(S1, S2: PLPMChar; L1, L2: Int32): PLPMChar; static;
	private
		{ Check that capture is valid }
		function CheckCatpure(L: Int32; out Res: Int32): TLPMResult;
		{ Return the latest capture that pends closing }
		function CaptureToClose(out Res: Int32): TLPMResult;
	private
		{ Return pointer to the next character after end of class }
		function ClassEnd(P: PLPMChar; out Res: PLPMChar): TLPMResult;
	private
		{ Match balancing (%bxy form in pattern) }
		function MatchBalance(S: PLPMChar; P: PLPMChar; out Res: PLPMChar): TLPMResult;
		{ Match special character class (%d, %a, %h etc) }
		function MatchClass(C, CL: TLPMChar): Boolean;
		{ Match character class ([abc], [^a-Z] etc) }
		function MatchBracketClass(C: TLPMChar; P: PLPMChar; EC: PLPMChar): Boolean;
		{ Match capture ((), (abc+) etc) }
		function MatchCapture(S: PLPMChar; L: Int32; out Res: PLPMChar): TLPMResult;
		{ Does match single char using current pattern? }
		function SingleMatch(S: PLPMChar; P: PLPMChar; EP: PLPMChar): Boolean;
		{ Tries to find as much characters matching pattern as it can to match the pattern }
		function MaxExpand(S: PLPMChar; P: PLPMChar; EP: PLPMChar; out Res: PLPMChar): TLPMResult;
		{ Tries to find as least characters matching pattern as it can to match the pattern }
		function MinExpand(S: PLPMChar; P: PLPMChar; EP: PLPMChar; out Res: PLPMChar): TLPMResult;
	private
		{ Starts new capture with specified index (just note - What is Index + Ord('1'), just because it is used with characters in the match function) }
		function StartCapture(S: PLPMChar; P: PLPMChar; What: Int32; out Res: PLPMChar): TLPMResult;
		{ Ends last capture, defining its length and position }
		function EndCapture(S: PLPMChar; P: PLPMChar; out Res: PLPMChar): TLPMResult;
	private
		{ Main function used to match pattern in input string
			Requires attuned record (Nullify + SrcInit + SrcEnd + PEnd), so better use Find method
		}
		function Match(S: PLPMChar; P: PLPMChar; out Res: PLPMChar): TLPMResult;
	public
		{ Wrapper around Match method, that will do most of dirty work for you
			SourceStr - pointer to first char in source string
			PatternStr - pointer to first char in pattern string
			SourceStrLength - length of source string IN CHARS, NOT IN BYTES
			PatternStrLength - length of pattern string IN CHARS, NOT IN BYTES
			PosToStart - where function must start from; it is 0-based!!!
			out Res - output is pointer to first char where pattern was found or nil, if pattern was not found
		}
		function Find(SourceStr: PLPMChar; PatternStr: PLPMChar; SourceStrLength: Int32; PatternStrLength: Int32; PosToStart: Int32; out Res: PLPMChar): TLPMResult; overload;
		{ Object pascal proxy method
			SourceStr - first string
			PatternStr - second string
			PosToStart - position to start from (it is 1-based!!!)
		}
		function Find(SourceStr, PatternStr: TLPMString; PosToStart: Int32): TLPMResult; overload;
	public
		function GetCaptureContent(const Index: Int32): TLPMString;
	private
		{ Method to clear internal structure and nullify all the record fields }
		procedure Nullify;
	end;

{$PointerMath On}

implementation

{$IfNDef UseStaticRanges}
uses
	Character;
{$EndIf}

{ TLPMMatchState }

class function TLPMMatchState.NoSpecials(P: PLPMChar; L: Int32): Boolean;
var i: Integer;
begin
	for i := 0 to L - 1 do
		if P[i] in LPM_SPECIALS then begin
			Result := False;
			Exit;
		end;
	Result := True;
end;

class function TLPMMatchState.LMemFind(S1, S2: PLPMChar; L1, L2: Int32): PLPMChar;
var Init: PLPMChar;
	i: Int32;
begin
	if L2 = 0 then begin
		{ Empty strings are everywhere }
		Result := S1;
		Exit;
	end
	else if L2 > L1 then begin
		{ Avoids negative L1 }
		Result := nil;
		Exit;
	end;

	L2 := L2 - 1;
	L1 := L1 - L2;
	while L1 > 0 do begin
		Init := nil;
		for i := 0 to L1 - 1 do
			if S1[i] = S2[0] then begin
				Init := S1 + i;
				Break;
			end;
		if Init = nil then
			Break;
		Init := Init + 1;
		if CompareByte(Init^, S2[1], L2 * SizeOf(TLPMChar)) = 0 then begin
			Result := Init - 1;
			Exit;
		end
		else begin
			L1 := L1 - (Init - S1);
			S1 := Init;
		end;
	end;
	{ Not found }
	Result := nil;
end;

function TLPMMatchState.CheckCatpure(L: Int32; out Res: Int32): TLPMResult;
begin
	L := L - Ord('1');
	if (L < 0) or (L >= Level) or (Captures[L].Length = LPM_CAP_UNFINISHED) then begin
		{#! Add exception info about index }
		Result := LPM_RESULT_INVALID_CAPTURE_INDEX;
		Exit;
	end;
	Result := LPM_RESULT_OK;
	Res := L;
end;

function TLPMMatchState.CaptureToClose(out Res: Int32): TLPMResult;
var i: Int32;
begin
	for i := Level - 1 downto 0 do
		if Captures[i].Length = LPM_CAP_UNFINISHED then begin
			Res := i;
			Result := LPM_RESULT_OK;
			Exit;
		end;
	Result := LPM_RESULT_INVALID_PATTERN_CAPTURE;
end;

function TLPMMatchState.ClassEnd(P: PLPMChar; out Res: PLPMChar): TLPMResult;
begin
	Result := LPM_RESULT_OK;
	P := P + 1;
	case (P - 1)^ of
		LPM_L_ESC:
			begin
				if P = PEnd then begin
					Result := LPM_RESULT_CLASS_END_ENDS_WITH_ESCAPE_CHAR;
					Exit;
				end;
				Res := P + 1;
				Exit;
			end;
		'[':
			begin
				if P[0] = '^' then
					P := P + 1;
				repeat
					if P = PEnd then begin
						Result := LPM_RESULT_CLASS_DOES_NOT_END_WITH_SQUARE_BRACKET;
						Exit;
					end;
					P := P + 1;
					if (P[0] = LPM_L_ESC) and (P < PEnd) then
						{ Skip escapes }
						P := P + 1;
				until P[0] = ']';
				Res := P + 1;
			end;
	else
		Res := P;
	end;
end;

function TLPMMatchState.MatchBalance(S: PLPMChar; P: PLPMChar; out Res: PLPMChar): TLPMResult;
var B, E: TLPMChar;
	Cont: Int32;
begin
	if P >= (PEnd - 1) then begin
		Result := LPM_RESULT_MISSING_ARGUMENTS_FOR_BALANCE;
		Exit;
	end;
	Result := LPM_RESULT_OK;
	if not (S[0] = P[0]) then begin
		{ Does not match }
		Res := nil;
		Exit;
	end;

	B := P[0];
	E := P[1];
	Cont := 1;
	S := S + 1;
	while S < SrcEnd do begin
		if S[0] = E then begin
			Cont := Cont - 1;
			if Cont <= 0 then begin
				Res := S + 1;
				Exit;
			end;
		end
		else if S[0] = B then
			Cont := Cont + 1;
		S := S + 1;
	end;
	Res := nil;
end;

function TLPMMatchState.MatchClass(C, CL: TLPMChar): Boolean;
begin
	{$IfNDef UseStaticRanges}
	case CL of
		'a', 'A': Result := (CL = 'A') xor IsLetter(UnicodeChar(C));
		'c', 'C': Result := (CL = 'C') xor IsControl(UnicodeChar(C));
		'd', 'D': Result := (CL = 'D') xor IsDigit(UnicodeChar(C));
		'g', 'G': Result := (CL = 'G') xor not IsWhiteSpace(UnicodeChar(C));
		'l', 'L': Result := (CL = 'L') xor IsLower(UnicodeChar(C));
		'p', 'P': Result := (CL = 'P') xor IsPunctuation(UnicodeChar(C));
		's', 'S': Result := (CL = 'S') xor IsWhiteSpace(UnicodeChar(C));
		'u', 'U': Result := (CL = 'U') xor IsUpper(UnicodeChar(C));
		'w', 'W': Result := (CL = 'W') xor IsLetterOrDigit(UnicodeChar(C));
		'x', 'X': Result := (CL = 'X') xor (C in ['0'..'9', 'a'..'f', 'A'..'F']);
		'z', 'Z': Result := (CL = 'Z') xor (C = #0);
	else
		Result := CL = C;
	end;
	{$Else}
	case CL of
		'a', 'A': Result := (CL = 'A') xor (((C >= 'a') and (C <= 'z') or ((C >= 'A') or (C <= 'Z'))));
		'c', 'C': Result := (CL = 'C') xor (C < ' ');
		'd', 'D': Result := (CL = 'D') xor (C in ['0'..'9']);
		'g', 'G': Result := (CL = 'G') xor not (C <= ' ');
		'l', 'L': Result := (CL = 'L') xor ((C >= 'a') and (C <= 'z'));
		'p', 'P': Result := (CL = 'P') xor (((C >= '!') and (C < '0')) or ((C > '9') and (C < 'A')) or ((C > 'Z') and (C < 'a')) or ((C > 'z') and (C < #128)));
		's', 'S': Result := (CL = 'S') xor (C <= ' ');
		'u', 'U': Result := (CL = 'U') xor ((C >= 'A') and (C <= 'Z'));
		'w', 'W': Result := (CL = 'W') xor ((C in ['0'..'9']) or (((C >= 'a') and (C <= 'z')) or ((C >= 'A') and (C <= 'Z'))));
		'x', 'X': Result := (CL = 'X') xor (C in ['0'..'9', 'a'..'f', 'A'..'F']);
		'z', 'Z': Result := (CL = 'Z') xor (C = #0);
	else
		Result := CL = C;
	end;
	{$EndIf}
end;

function TLPMMatchState.MatchBracketClass(C: TLPMChar; P: PLPMChar; EC: PLPMChar): Boolean;
var Sig: Boolean;
begin
	Sig := True;
	if P[1] = '^' then begin
		Sig := False;
		{ Skip the ^ }
		P := P + 1;
	end;
	P := P + 1;
	while P < EC do begin
		if P[0] = LPM_L_ESC then begin
			P := P + 1;
			if MatchClass(C, P[0]) then begin
				Result := Sig;
				Exit;
			end;
		end
		else if (P[1] = '-') and ((P + 2) < EC) then begin
			P := P + 2;
			if (C >= (P - 2)^) and (C <= P[0]) then begin
				Result := Sig;
				Exit;
			end;
		end
		else if P[0] = C then begin
			Result := Sig;
			Exit;
		end;
		P := P + 1;
	end;
	Result := not Sig;
end;

function TLPMMatchState.MatchCapture(S: PLPMChar; L: Int32; out Res: PLPMChar): TLPMResult;
var Len: Int32;
begin
	Result := CheckCatpure(L, L);
	if LongBool(Result) then
		Exit;
	Len := Captures[L].Length;
	if ((SrcEnd - S) >= Len) and (CompareChar(Captures[L].Init^, S^, Len * SizeOf(TLPMChar)) = 0) then
		Res := S + Len
	else
		Res := nil;
end;

function TLPMMatchState.SingleMatch(S: PLPMChar; P: PLPMChar; EP: PLPMChar): Boolean;
var C: TLPMChar;
begin
	if S >= SrcEnd then begin
		Result := False;
		Exit;
	end;

	C := S[0];
	case P[0] of
		'.': Result := True;
		LPM_L_ESC: Result := MatchClass(C, P[1]);
		'[': Result := MatchBracketClass(C, P, EP - 1);
	else
		Result := C = P[0];
	end;
end;

function TLPMMatchState.MaxExpand(S: PLPMChar; P: PLPMChar; EP: PLPMChar; out Res: PLPMChar): TLPMResult;
var i: Int32;
	AlsoRes: PLPMChar;
begin
	Result := LPM_RESULT_OK;
	{ Count maximum expand for an item }
	i := 0;
	while SingleMatch(S + i, P, EP) do
		i := i + 1;
	{ Keep trying to match with maximum repetitions }
	while i >= 0 do begin
		Result := Match(S + i, EP + 1, AlsoRes);
		if LongBool(Result) then
			Exit;
		if not (AlsoRes = nil) then begin
			Res := AlsoRes;
			Exit;
		end;
		{ Else did not match - reduce 1 repetitions to try again }
		i := i - 1;
	end;
	Res := nil;
end;

function TLPMMatchState.MinExpand(S: PLPMChar; P: PLPMChar; EP: PLPMChar; out Res: PLPMChar): TLPMResult;
var AlsoRes: PLPMChar;
begin
	while not False do begin
		Result := Match(S, EP + 1, AlsoRes);
		if LongBool(Result) then
			Exit;
		if not (AlsoRes = nil) then begin
			Res := AlsoRes;
			Exit;
		end
		else if SingleMatch(S, P, EP) then begin
			{ Try with one more repetitions }
			S := S + 1;
		end
		else begin
			Res := nil;
			Exit;
		end;
	end;
end;

function TLPMMatchState.StartCapture(S: PLPMChar; P: PLPMChar; What: Int32; out Res: PLPMChar): TLPMResult;
var AlsoRes: PLPMChar;
	Level: Int32;
begin
	Level := Self.Level;
	if Level >= LPM_MAX_CAPTURES then begin
		Result := LPM_RESULT_TOO_MANY_CAPTURES;
		Exit;
	end;
	Captures[Level].Init := S;
	Captures[Level].Length := What;
	Self.Level := Level + 1;
	Result := Match(S, P, AlsoRes);
	if LongBool(Result) then
		Exit;
	if AlsoRes = nil then
		Dec(Self.Level);
	Res := AlsoRes;
end;

function TLPMMatchState.EndCapture(S: PLPMChar; P: PLPMChar; out Res: PLPMChar): TLPMResult;
var L: Int32;
	AlsoRes: PLPMChar;
begin
	Result := CaptureToClose(L);
	if LongBool(Result) then
		Exit;
	{ Close capture }
	Captures[L].Length := S - Captures[L].Init;
	Result := Match(S, P, AlsoRes);
	if LongBool(Result) then
		Exit;
	if AlsoRes = nil then
		{ Undo capture }
		Captures[L].Length := LPM_CAP_UNFINISHED;
	Res := AlsoRes;
end;

function TLPMMatchState.Match(S: PLPMChar; P: PLPMChar; out Res: PLPMChar): TLPMResult;
var EP, AlsoRes: PLPMChar;
	Previous: TLPMChar;
begin
	if not LongBool(MatchDepth) then begin
		Result := LPM_RESULT_PATTERN_IS_TOO_COMPLEX;
		Exit;
	end;
	Dec(MatchDepth);

	if Level = 0 then begin
		Result := StartCapture(S, P, LPM_CAP_UNFINISHED, S);
		if LongBool(Result) then
			Exit;
		if not (S = nil) then begin
			Captures[0].Length := S - Captures[0].Init;
			Res := Captures[0].Init;
		end
		else
			Res := nil;

		Inc(MatchDepth);
		Exit;
	end;

	{ Infinite loop (well, there are no flow paths that makes it infinite) is used to be able to Continue the execution,
		It works the same as goto would work, but it is prettier and procedural than goto approach is
	}
	while not False do begin
		if P = PEnd then
			Break;
		case P[0] of
			'(':
				begin
					{ Start capture }
					if P[1] = ')' then
						{ Position capture }
						Result := StartCapture(S, P + 2, LPM_CAP_POSITION, S)
					else
						Result := StartCapture(S, P + 1, LPM_CAP_UNFINISHED, S);
					if LongBool(Result) then
						Exit;
					Break;
				end;
			')':
				begin
					Result := EndCapture(S, P + 1, S);
					if LongBool(Result) then
						Exit;
					Break;
				end;
			'$':
				begin
					if (p[1] = PEnd) then begin
						if not (S = SrcEnd) then
							S := nil;
					end
					else begin
						{ It will end case and go to default }
					end;
				end;
			LPM_L_ESC:
				begin
					case P[1] of
						'b':
							begin
								{ Balanced string }
								Result := MatchBalance(S, P + 2, S);
								if LongBool(Result) then
									Exit;
								if not (S = nil) then begin
									P := P + 4;
									Continue;
								end;
								Break;
							end;
						'f':
							begin
								P := P + 2;
								if not (P[0] = '[') then begin
									Result := LPM_RESULT_PATTERN_EXPECTED_SQUARE_BRACKET_AFTER_FRONTIER;
									Exit;
								end;
								Result := ClassEnd(P, Ep);
								if LongBool(Result) then
									Exit;
								if S = SrcInit then
									Previous := #0
								else
									Previous := (S - 1)^;
								if not LongBool(MatchBracketClass(Previous, P, EP - 1)) and LongBool(MatchBracketClass(S[0], P, EP - 1)) then begin
									P := EP;
									{ Go to init }
									Continue;
								end;
								{ Match failed }
								S := nil;
								Break;
							end;
						'0', '1', '2', '3', '4', '5', '6', '7', '8', '9':
							begin
								{ Capture results (%0 - %9) }
								Result := MatchCapture(S, Ord(P[1]), S);
								if LongBool(Result) then
									Exit;
								if not (S = nil) then begin
									P := P + 2;
									Continue;
								end;
								Break;
							end;
					end;
					{ Else go to default }
				end;

		end;
		{ Default }
		Result := ClassEnd(P, EP);
		if not SingleMatch(S, P, EP) then begin
			if (EP[0] = '*') or (EP[0] = '?') or (EP[0] = '-') then begin
				{ Accept empty }
				P := EP + 1;
				{ Go to init }
				Continue;
			end
			else
				{ '+' or no suffix - fail }
				S := nil;
		end
		else begin
			{ Matched once }
			{ Handle optional suffix }
			case EP[0] of
				'?':
					begin
						Result := Match(S + 1, EP + 1, AlsoRes);
						if LongBool(Result) then
							Exit;
						if not (AlsoRes = nil) then
							S := AlsoRes
						else begin
							P := EP + 1;
							{ Go to init }
							Continue;
						end;
						Break;
					end;
				'+', '*':
					begin
						if EP[0] = '+' then
							{ 1 or more repetitions and 1 match already done }
							S := S + 1;
						{ 0 or more repetitions }
						Result := MaxExpand(S, P, EP, S);
						if LongBool(Result) then
							Exit;
						Break;
					end;
				'-':
					begin
						{ 0 or more repetitions (minimum) }
						Result := MinExpand(S, P, EP, S);
						if LongBool(Result) then
							Exit;
						Break;
					end;
			else
				{ No suffix }
				S := S + 1;
				P := EP;
				{ Go to init }
				Continue;
			end;
		end;
		Break;
	end;
	Inc(MatchDepth);
	Res := S;
	Result := LPM_RESULT_OK;
end;

function TLPMMatchState.Find(SourceStr: PLPMChar; PatternStr: PLPMChar; SourceStrLength: Int32; PatternStrLength: Int32; PosToStart: Int32; out Res: PLPMChar): TLPMResult;
var Ps: PLPMChar;
	Anchor: Boolean;
	i: Integer;
begin
	Result := LPM_RESULT_OK;
	Res := nil;

	Nullify;
	if (PosToStart >= SourceStrLength) or (PosToStart < 0) then
		Exit;
	if NoSpecials(PatternStr, PatternStrLength) then begin
		{ No special symbols found, using simple Pos }
		Ps := LMemFind(SourceStr + PosToStart, PatternStr, SourceStrLength - PosToStart, PatternStrLength);
		if not (Ps = nil) then begin
			{ Using captures }
			Captures[0].Init := Ps;
			Captures[0].Position := Ps - SourceStr;
			Captures[0].Length := PatternStrLength;
			Level := Level + 1;
			Res := Ps;
		end;
		{ Else there are no captures }
		Exit;
	end;

	Ps := SourceStr + PosToStart;
	Anchor := PatternStr[0] = '^';
	if Anchor then begin
		{ Skip anchor }
		PatternStr := PatternStr + 1;
		PatternStrLength := PatternStrLength - 1;
	end;

	Self.SrcInit := SourceStr;
	Self.SrcEnd := SourceStr + SourceStrLength;
	Self.PEnd := PatternStr + PatternStrLength;

	{ If anything will be found - we are assuming this will be written to the 0th capture }
	repeat
		Self.Level := 0;
		Result := Match(Ps, PatternStr, Res);
		if LongBool(Result) then
			Exit;
		if not (Res = nil) then
			Break;
		Ps := Ps + 1;
	until (Ps >= SrcEnd) or Anchor;

	for i := 0 to Level - 1 do
		Captures[i].Position := Captures[i].Init - SourceStr;
end;

function TLPMMatchState.Find(SourceStr, PatternStr: TLPMString; PosToStart: Int32): TLPMResult;
var DummyRes: PLPMChar;
	i: Integer;
begin
	Result := Find(PLPMChar(Pointer(SourceStr)), PLPMChar(Pointer(PatternStr)), Length(SourceStr), Length(PatternStr), PosToStart - 1, DummyRes);
	if LongBool(Result) then
		Exit;
	for i := 0 to Level - 1 do
		{ Because positions in pascal strings are 1-based }
		Inc(Captures[i].Position);
end;

function TLPMMatchState.GetCaptureContent(const Index: Int32): TLPMString;
begin
	Result := '';
	if (Index >= 0) and (Index < Level) and (Captures[Index].Length > 0) then begin
		SetLength(Result, Captures[Index].Length);
		Move(Captures[Index].Init^, Result[1], Captures[Index].Length * SizeOf(TLPMChar));
	end;
end;

procedure TLPMMatchState.Nullify;
begin
	Self.Level := 0;
	Self.MatchDepth := LPM_MAX_CALLS;
	Self.PEnd := nil;
	Self.SrcInit := nil;
	Self.SrcEnd := nil;
end;

end.


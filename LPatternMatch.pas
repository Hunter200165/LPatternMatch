program LPatternMatch;

{$IfDef FPC}
	{$Mode ObjFPC}
{$Else}
	{$Apptype Console}
{$EndIf}
{$PointerMath On}

uses LPatternMatch.Core;

var LPM: TLPMMatchState;
    A, B: UnicodeString;
    Status: TLPMResult;
    i: Integer;
begin
    while not False do begin
		Write('Input string : ');
		ReadLn(A);
		Write('Input pattern: ');
		ReadLn(B);
		if (Length(A) = 0) or (Length(B) = 0) then begin
			WriteLn('Either A or B is empty - exiting');
			Exit;
		end;
		// Status := LPM.Find(@A[1], @B[1], Length(A), Length(B), 0, Res);
		Status := LPM.Find(A, B, 1);
		if LongBool(Status) then begin
			WriteLn('Finished with error status = ', Status);
		end
		else begin
			if LPM.Level <= 0 then
				WriteLn('Not found')
			else begin
				for i := 0 to LPM.Level - 1 do begin
					WriteLn('Capture #', i, ': ');
					WriteLn('	Starts : ', LPM.Captures[i].Position);
					WriteLn('	Length : ', LPM.Captures[i].Length);
					WriteLn('	Content: ', LPM.GetCaptureContent(i));
				end;
			end;
		end;
	end;
end.


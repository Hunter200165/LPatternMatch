program LPatternMatch;

{$Mode ObjFPC}
{$PointerMath On}

uses LPatternMatch.Core;

var LPM: TLPMMatchState;
	A, B: UnicodeString;
	Res: PLPMChar;
	Status: TLPMResult;
	i, k: Integer;
begin
	while not False do begin
		Write('Input A: ');
		ReadLn(A);
		Write('Input pattern: ');
		ReadLn(B);
		if (Length(A) = 0) or (Length(B) = 0) then begin
			WriteLn('Either A or B is empty - exiting');
			Exit;
		end;
		Status := LPM.Find(@A[1], @B[1], Length(A), Length(B), 0, Res);
		if LongBool(Status) then begin
			WriteLn('Finished with error status = ', Status);
		end
		else begin
			if LPM.Level <= 0 then
				WriteLn('Not found')
			else begin
				for i := 0 to LPM.Level - 1 do begin
					WriteLn('Capture #', i, ': ');
					WriteLn('	Starts : ', LPM.Captures[i].Init - PLPMChar(@A[1]));
					WriteLn('	Length : ', LPM.Captures[i].Length);
					Write('	Content: ');
					for k := 0 to LPM.Captures[i].Length - 1 do
						Write(LPM.Captures[i].Init[k]);
					WriteLn;
				end;
			end;
		end;
	end;
end.


-- PROCEDURE MNESON.TOOLS.DUMP (BODY ONLY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

procedure Mneson.Tools.Dump is
   procedure Dump_Link (Source, Target : Vertex) is
   begin
      Ada.Text_IO.Put_Line (Img (Source) & " " & Img (Target));
   end;
begin
   Open_Argument_1;
   For_Each_Link (Dump_Link'Unrestricted_Access);
   Close;
end;
   
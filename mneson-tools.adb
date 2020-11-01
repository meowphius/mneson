-- PACKAGE MNESON.TOOLS (BODY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

package body Mneson.Tools is

   procedure Open_Argument_1 is
      use Ada.Text_IO;
   begin
      Open (Ada.Command_Line.Argument (1));
   exception
      when others =>
         Put_Line
           (Standard_Error,
            "Error opening graph. " &
            "Make sure the first argument is a valid name.");
         raise;
   end;

end;

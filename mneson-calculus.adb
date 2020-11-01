-- PACKAGE MNESON.CALCULUS (GENERIC BODY)
-- (C) Marius Amado Alves (amado.alves@netcabo.pt)
-- License: SDC Conditions (www.softdevelcoop.org)

with Mneson.Structures;

package body Mneson.Calculus is

   package Structures is new Mneson.Structures (Work);
   use Structures;
   use Work;

   Element_AT   : constant Vertex := To_Vertex ("element" );
   Intersect_AT : constant Vertex := To_Vertex ("intersct");
   Set_AT       : constant Vertex := To_Vertex ("set"     );
   Set_1_AT     : constant Vertex := To_Vertex ("set_1"   );
   Set_2_AT     : constant Vertex := To_Vertex ("set_2"   );
   Singleton_AT : constant Vertex := To_Vertex ("singletn");
   Sources_AT   : constant Vertex := To_Vertex ("sources" );
   Subtract_AT  : constant Vertex := To_Vertex ("subtract");
   Targets_AT   : constant Vertex := To_Vertex ("targets" );

   function To_Selection (Set : Vertex) return Selection is
   begin
      return New_Transient_Record
        ((Set_AT,
          New_Instance (Set_AT, Set)));
   end;

   function Singleton (X : Vertex) return Selection is
   begin
      return New_Transient_Record
        ((Singleton_AT,
          New_Instance (Element_AT, X)));
   end;

   function Targets (Set : Selection) return Selection is
   begin
      return New_Transient_Record
        ((Targets_AT,
          New_Instance (Set_AT, Set)));
   end;

   function Sources (Set : Selection) return Selection is
   begin
      return New_Transient_Record
        ((Sources_AT,
          New_Instance (Set_AT, Set)));
   end;

   function Intersect (Set_1, Set_2 : Selection) return Selection is
   begin
      return New_Transient_Record
        ((Intersect_AT,
          New_Instance (Set_1_AT, Set_1),
          New_Instance (Set_2_AT, Set_2)));
   end;

   function Subtract (Set_1, Set_2 : Selection) return Selection is
   begin
      return New_Transient_Record
        ((Subtract_AT,
          New_Instance (Set_1_AT, Set_1),
          New_Instance (Set_2_AT, Set_2)));
   end;

   procedure For_Each (Set : Selection; Process : Process_Vertex) is
   begin
      if Connected (Set, Set_AT) then
         For_Each_Target (Get_Value (Set, Set_AT), Process);
      elsif Connected (Set, Singleton_AT) then
         Process (Get_Value (Set, Element_AT));
      elsif Connected (Set, Targets_AT) then
         declare
            procedure Process_Targets (X : Vertex) is
            begin
               For_Each_Target (X, Process);
            end;
         begin
            For_Each
              (Get_Value (Set, Set_AT),
               Process_Targets'Unrestricted_Access);
         end;
      elsif Connected (Set, Sources_AT) then
         declare
            procedure Process_Sources (X : Vertex) is
            begin
               For_Each_Source (X, Process);
            end;
         begin
            For_Each
              (Get_Value (Set, Set_AT),
               Process_Sources'Unrestricted_Access);
         end;
      elsif Connected (Set, Intersect_AT) then
         declare
            Set_2 : Selection := Get_Value (Set, Set_2_AT);
            procedure Process_If_In_Set_2 (X : Vertex) is
            begin
              if Is_In (X, Set_2) then Process (X); end if;
            end;
         begin
            For_Each
              (Get_Value (Set, Set_1_AT),
               Process_If_In_Set_2'Unrestricted_Access);
         end;
      elsif Connected (Set, Subtract_AT) then
         declare
            Set_2 : Selection := Get_Value (Set, Set_2_AT);
            procedure Process_If_Not_In_Set_2 (X : Vertex) is
            begin
              if not Is_In (X, Set_2) then Process (X); end if;
            end;
         begin
            For_Each
              (Get_Value (Set, Set_1_AT),
               Process_If_Not_In_Set_2'Unrestricted_Access);
         end;
      else
         raise Type_Error;
      end if;
   end;

   function Is_In (X : Vertex; Set : Selection) return Boolean is -- O(#Set)
      Yes : exception;
      procedure Compare (Element : Vertex) is
      begin
         if X = Element then raise Yes; end if;
      end;
   begin
      For_Each (Set, Compare'Unrestricted_Access);
      return False;
   exception
      when Yes => return True;
   end;

   function Extract
     (Set : Selection; Index : Positive_64 := 1) return Vertex -- O(Index)
   is
      Result : Vertex;
      N : Natural_64 := 0;
      Done : exception;
      procedure Return_Indexed (Element : Vertex) is
      begin
         N := N + 1;
         if N = Index then
            Result := Element;
            raise Done;
         end if;
      end;
   begin
      For_Each (Set, Return_Indexed'Unrestricted_Access);
      raise Constraint_Error;
   exception
      when Done => return Result;
   end;

end;

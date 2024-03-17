----------------------------------------------------------------------------------
-- Author: Tarek Eldeeb
--
-- Create Date:    18:48:57 12/11/2018
-- Design Name:
-- Module Name:    PE - Behavioral
-- Project Name:
-- Target Devices:
-- License:  Check "LICENSE" which comes with this distribution for more informationg.
-- Description:
--
-- Dependencies:
--
-- Revision:
-- Revision 0.01 - File Created
-- Additional Comments:
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
use IEEE.NUMERIC_STD.ALL;
use IEEE.math_real.all;

library matrix_mul_v1_00_a;
use matrix_mul_v1_00_a.ai_util_pkg.all;

entity PE is
  Generic (
    n            : integer := 3;
    width        : integer := 8;
    id           : integer := 0;
    verbose      : boolean := false);
  Port (
    clk          : in  STD_LOGIC;
    rst          : in  STD_LOGIC;
    rst_C        : in  STD_LOGIC;
    rst_out      : out STD_LOGIC;
    rst_C_out    : out STD_LOGIC;
    A            : in  STD_LOGIC_VECTOR (width-1 downto 0);
    B            : in  STD_LOGIC_VECTOR (width-1 downto 0);
    AB_val       : in  STD_LOGIC;
    AB_count     : in  unsigned(ilog2(n+1)-1 downto 0);
    AB_count_out : out unsigned(ilog2(n+1)-1 downto 0);
    C            : in  STD_LOGIC_VECTOR (width-1 downto 0);
    C_val        : in  STD_LOGIC;
    Cout_Rows    : in  STD_LOGIC;
    OCC_count    : in  unsigned(ilog2(n+1)-1 downto 0);
    OCR_count    : in  unsigned(ilog2(n+1)-1 downto 0);
    A_out        : out STD_LOGIC_VECTOR (width-1 downto 0);
    B_out        : out STD_LOGIC_VECTOR (width-1 downto 0);
    ABout_val    : out STD_LOGIC;
    C_out        : out STD_LOGIC_VECTOR (width-1 downto 0);
    Cout_val     : out STD_LOGIC);
end PE;

architecture Behavioral of PE is

constant  pipedelay   : integer:= 2;

signal A_reg, B_reg, BM_reg, BL_reg    : STD_LOGIC_VECTOR (width-1 downto 0);
signal count_n, count_n_out, count_n2  : unsigned (ilog2(n+1)-1 downto 0);
signal run, sel_BL, Cout_val_o         : STD_LOGIC;
signal col_ready, row_ready           : STD_LOGIC;
signal outputting_reg, output_en      : STD_LOGIC;
type C_col_T is array (0 to n-1) of std_logic_vector(width-1 downto 0);
signal C_col : C_col_T;
shared variable outputting      : STD_LOGIC:='0';

begin

process(clk,rst)
  variable count               : integer range 0 to n-1;
  variable B_sel, res          : STD_LOGIC_VECTOR (width-1 downto 0);
  variable prod                : STD_LOGIC_VECTOR (2*width-1 downto 0);
begin

  if rising_edge(clk) then
    rst_out        <= rst;
    rst_C_out       <= rst_C;
    AB_count_out    <= AB_count;
    if rst = '1' then
      A_reg        <= (others=>'0');
      B_reg        <= (others=>'0');
      C_out        <= (others=>'0');
      Cout_val_o  <= '0';
      BM_reg      <= (others=>'0');
      BL_reg      <= (others=>'0');
      ABout_val    <= '0';
      count_n      <= (others=>'0');
      count_n_out  <= (others=>'0');
      count_n2     <= (others=>'0');
      outputting_reg  <= '0';
      outputting   := '0';
      sel_BL       <= '0';
      col_ready    <= '0';
      row_ready    <= '0';
      if rst_C = '1' then
        C_col      <= (others=>(others=>'0'));
      end if;
    else
    -- Counters: Run with Valid input or output
    if OCC_count>id and (AB_val = '1' or outputting = '1') then
      if count_n = n-1 then
        count_n  <= (others=>'0');
        sel_BL  <= '1';
        if count_n2 = n then --  NOTE: Can be "n-1" for earlier output
                             --        But no pauses are accepted when (count_n2 = n-1) then.
          count_n2    <= (others=>'0');
          outputting  := '1';
        else
          count_n2  <= count_n2 + 1;
        end if;
      else
        count_n  <= count_n + 1;
      end if;
    end if;

    -- Dataflow: Run with Valid input
    if AB_val = '1' then
      A_reg    <= A;
      B_reg    <= B;
      ABout_val  <= '1';
      if run = '1' then
        count  := to_integer(count_n);
        if sel_BL = '1' then
          B_sel  := BL_reg;
        else
          B_sel  := BM_reg;
        end if;
        if verbose then
          report "[" & integer'image(id) & "]"
              &" idx="   & integer'image(count)
              &"  A="     & integer'image(to_integer(signed(A)))
              &"  B="     & integer'image(to_integer(signed(B_sel)));
        end if;    
        prod      := std_logic_vector(signed(A)*signed(B_sel));
        -- TODO: Handle overflows!
        res      := std_logic_vector(signed(C_col(count)) + signed(prod(width-1 downto 0)));
        C_col(count)  <= res;
      end if;

      if run = '1' and count_n = n-1 then
        sel_BL  <= not(sel_BL);
      end if;
      if count_n = to_unsigned(id,ilog2(n)) then
        if sel_BL = '1' then
          BM_reg  <= B;
        else
          BL_reg  <= B;
        end if;
      end if;
    else
      ABout_val  <= '0';
    end if;

    -- Outputs: Run unconditionally
    if outputting = '1'
        and count_n = to_unsigned(((n-pipedelay)*id) mod n,ilog2(n))
        and count_n2 = to_unsigned(((n-pipedelay)*id)/n,ilog2(n)) then
      col_ready  <= '1';
    end if;
    if outputting_reg = '1'
        and count_n = to_unsigned((n-id-pipedelay+1) mod n,ilog2(n)) then
      row_ready  <= '1';
    else
      row_ready  <= '0';
    end if;
    if C_val = '1' then
	   if count_n_out = to_unsigned(n-1,ilog2(n+1))  then
		  count_n_out  <= (others=>'0');
      else
	     count_n_out  <= count_n_out + 1;
		end if;  
    end if;
	 
    if (Cout_Rows = '0' and col_ready = '1') OR (Cout_Rows = '1' and row_ready = '1')  then
      if count_n_out = to_unsigned(n-1,ilog2(n+1))  then
        count_n_out    <= (others=>'0');
        col_ready     <= '0';
        row_ready     <= '0';
        outputting    := '0';
      else
        count_n_out  <= count_n_out + 1;
      end if;
      C_out    <= C_col(to_integer(count_n_out));
    else
      C_out    <= C;
    end if;
	 
	 if id>0 or count_n_out < OCR_count then --Not the best implementation.
		Cout_val_o  <=  ( not(Cout_Rows) and col_ready )
							  OR ( Cout_Rows and row_ready )
							  OR C_val;
	 else
	   Cout_val_o	<= '0';
    end if;
	 
    outputting_reg  <= outputting;
  end if;
 end if;
end process;

run       <= '1' when OCC_count>id and AB_val='1' and ((count_n2>0 and count_n2 <= AB_count) OR outputting='1') else
          '0';
output_en  <= '1' when outputting='1' and (count_n2>0 OR AB_val='1')else
          '0';
A_out     <= A_reg;
B_out     <= B_reg;
Cout_val    <= Cout_val_o;
  
end Behavioral;

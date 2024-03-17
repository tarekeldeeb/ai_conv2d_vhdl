----------------------------------------------------------------------------------
-- Author: Tarek ELDEEB
--
-- Create Date:    10:15:07 12/26/2018
-- Design Name:
-- Module Name:    mm - Behavioral
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
use ieee.std_logic_misc.all;

library matrix_mul_v1_00_a;
use matrix_mul_v1_00_a.ai_util_pkg.all;
--use UNISIM.VComponents.all;

entity mm is
	 Generic (
			  n			: integer := 49; 	-- Number of PEs, and implicitly the matrix size of A and B (n*n).
			  width		: integer := 16);  -- Data width, assuming signed integers
    Port ( clk 		: in  STD_LOGIC;
           rst 		: in  STD_LOGIC;
           rst_C 		: in  STD_LOGIC;
           A 			: in  STD_LOGIC_VECTOR (width-1 downto 0); -- Feed-in Column-based
           B 			: in  STD_LOGIC_VECTOR (width-1 downto 0); -- Feed-in Row-based
			  AB_val		: in  STD_LOGIC;                           -- Both A and B are valid inputs
			  AB_count  : in  unsigned(ilog2(n+1)-1 downto 0);     -- N in all cases, except remainders
			  Cout_Rows	: in  STD_LOGIC;                           -- Select if the output is required in Row-based
			  OCC_count : in  unsigned(ilog2(n+1)-1 downto 0);     -- N in all cases, except remainders
			  OCR_count : in  unsigned(ilog2(n+1)-1 downto 0);     -- N in all cases, except remainders
			  C_out 		: out STD_LOGIC_VECTOR (width-1 downto 0); -- C stream GEMM result
			  Cout_val 	: out STD_LOGIC);                          -- Result C is valid
end mm;

architecture Behavioral of mm is

	type stage_t is array (0 to n) of std_logic_vector( width -1 downto 0);
	signal sA, sB, sC 	: stage_t:=((others=>(others=>'U')));
	type count_stage_t is array (0 to n) of unsigned(ilog2(n+1)-1 downto 0);
	signal sAB_count     : count_stage_t;
	signal sAB_val, sC_val: std_logic_vector(0 to n);
	signal sRst, sRst_C: std_logic_vector(0 to n);

begin

sRst(0)		<= rst;
sRst_C(0)	<= rst_C;
sAB_val(0)	<= AB_val;
sA(0)       <= A;
sB(0)       <= B;
C_out			<= sC(0);
Cout_val		<= sC_val(0);
sC(n)			<= (others=>'0');
sC_val(n)	<= '0';
sAB_count(0)<= AB_count;

GenPE: for i in 0 to n-1 generate
	PElem: entity matrix_mul_v1_00_a.PE GENERIC MAP (n,width,i)
				PORT MAP(
				  clk 		   => clk,
				  rst 		   => sRst(i),
				  rst_C		   => sRst_C(i),
				  rst_out	   => sRst(i+1),
              rst_C_out    => sRst_C(i+1),
				  AB_val		   => sAB_val(i),
				  AB_count     => sAB_count(i),
				  AB_count_out => sAB_count(i+1),
				  A 			   => sA(i),
				  B 			   => sB(i),
				  C			   => sC(i+1),
				  C_val		   => sC_val(i+1),
				  Cout_Rows	   => Cout_Rows,
				  OCC_count    => OCC_count,
				  OCR_count    => OCR_count,
				  A_out 		   => sA(i+1),
				  B_out 		   => sB(i+1),
				  ABout_val	   => sAB_val(i+1),
				  C_out 		   => sC(i),
				  Cout_val	   => sC_val(i));
	End Generate;
end Behavioral;


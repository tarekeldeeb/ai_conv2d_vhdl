----------------------------------------------------------------------------------
-- Author: Tarek ELDEEB
--
-- Create Date:    10:15:07 12/26/2018
-- Design Name:
-- Module Name:    dual-port RAM model - Behavioral
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
library ieee;
use ieee.std_logic_1164.all;

library matrix_mul_v1_00_a;
use matrix_mul_v1_00_a.ai_util_pkg.all;

entity true_dpram_sclk is
  generic(
      width		: integer:=8;
		size		: integer:=1024;
		init		:  mem_init_t := (OTHERS => (OTHERS => '0'))
  );
	port 
	(	
		clk		: in std_logic;
		-- synthesis translate_off
		reinit	: mem_init_t := (OTHERS => (OTHERS => '0'));
      -- synthesis translate_on 
		data_c	: in std_logic_vector(width-1 downto 0);
		addr_c	: in natural range 0 to size-1;
		we_c		: in std_logic := '1';
		addr_a	: in natural range 0 to size-1;
		addr_b	: in natural range 0 to size-1;
		q_a		: out std_logic_vector(width-1 downto 0);
		q_b		: out std_logic_vector(width-1 downto 0)
	);
	
end true_dpram_sclk;

architecture rtl of true_dpram_sclk is
	
	-- Build a 2-D array type for the RAM
	subtype word_t is std_logic_vector(width-1 downto 0);
	type memory_t is array(0 to size-1) of word_t;
	
	-- Initializer function
	FUNCTION init_mem (init_data: IN mem_init_t) RETURN memory_t IS
        VARIABLE return_arg : memory_t := (OTHERS => (OTHERS => '1'));
    BEGIN
        FOR i IN 0 TO size - 1 LOOP
            return_arg(i) := init_data(i)(width - 1 DOWNTO 0);
        END LOOP;
        
        RETURN return_arg;
    END FUNCTION init_mem;
	 
	-- Declare the RAM
	shared variable ram : memory_t:=init_mem(init);
	
	-- synthesis translate_off
	signal reinit_reg	: mem_init_t;
   -- synthesis translate_on 
	
begin

	-- Port A/C
	process(clk)
	begin
		if(rising_edge(clk)) then 
		  -- synthesis translate_off 
		  reinit_reg	<= reinit;
		  if reinit_reg /= reinit then
			 ram :=init_mem(reinit);
		  end if;
		  -- synthesis translate_on 
			if(we_c = '1') then
				ram(addr_c) := data_c;
			end if;
			q_a <= ram(addr_a);
		end if;
	end process;
	
	-- Port B
	process(clk)
	begin
		if(rising_edge(clk)) then
			q_b <= ram(addr_b);
		end if;
	end process;
end rtl;

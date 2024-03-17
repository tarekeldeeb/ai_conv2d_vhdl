----------------------------------------------------------------------------------
-- Author: Tarek ELDEEB
--
-- Create Date:    10:15:07 12/26/2018
-- Design Name:
-- Module Name:    matrix multiplier - Behavioral
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
use IEEE.numeric_std.ALL;
use IEEE.math_real.all;
use ieee.std_logic_misc.all;

library matrix_mul_v1_00_a;
use matrix_mul_v1_00_a.ai_util_pkg.all;

entity matmul_burst_control is
   Generic (
      n        : integer := 4;   -- Number of PEs, and implicitly the matrix size of A and B (n*n).
      width    : integer := 16); -- Data width, assuming signed integers
    Port (
      clk      : in  STD_LOGIC;
      rst      : in  STD_LOGIC;
      run      : in  STD_LOGIC;
      cntIn    : in  t_mem_control_in;
      cntOut   : out t_mem_control_out
      );
end matmul_burst_control;

architecture Behavioral of matmul_burst_control is

  constant counter_width  : integer:=16;--ilog2(max_input_size/n); --TODO: Handle!
  
  SIGNAL conf            : t_config;
  SIGNAL Cout_val        : STD_LOGIC;
  SIGNAL Empty_A         : STD_LOGIC;
  SIGNAL Empty_B         : STD_LOGIC;
  SIGNAL AB_val          : STD_LOGIC;
  SIGNAL Read_A_o        : STD_LOGIC;
  SIGNAL Read_B_o        : STD_LOGIC;
  SIGNAL rst_pe          : STD_LOGIC:='0';
  SIGNAL rst_pe_c        : STD_LOGIC:='0';
  SIGNAL run_complete_o  : STD_LOGIC:='0';
  SIGNAL IA_Address      : STD_LOGIC_VECTOR(awidth-1 downto 0);
  SIGNAL IB_Address      : STD_LOGIC_VECTOR(awidth-1 downto 0);
  SIGNAL OC_Address      : STD_LOGIC_VECTOR(awidth-1 downto 0);  
  SIGNAL OCC_count        : STD_LOGIC_VECTOR(counter_width-1 downto 0);
  SIGNAL OCR_count        : STD_LOGIC_VECTOR(counter_width-1 downto 0);
  SIGNAL AB_count        : STD_LOGIC_VECTOR(counter_width-1 downto 0);
  
  TYPE t_runState IS (mem_rd, mem_rd_wait, exec, mem_wr, mem_wr_wait, halt);
  SIGNAL run_state   : t_runState;  
  SIGNAL run_complete: std_logic:='0';

  SIGNAL inner_block_counter, max_innerblock_counter  : unsigned(counter_width-1 downto 0);
  SIGNAL col_block_counter, max_colblock_counter      : unsigned(counter_width-1 downto 0);
  SIGNAL row_block_counter, max_rowblock_counter      : unsigned(counter_width-1 downto 0);
  SIGNAL OC_Remainder_col, OC_Remainder_row           : unsigned(counter_width-1 downto 0);
  SIGNAL OC_Remainder_inner                           : unsigned(counter_width-1 downto 0);
  SIGNAL rowPart_n_counterA, rowPart_n_counterB       : unsigned(ilog2(n+1)-1 downto 0);
  SIGNAL rowPart_n_counterC, rowpart_n_counterc_reg   : unsigned(ilog2(n+1)-1 downto 0);
  SIGNAL n_counter                                    : unsigned(ilog2(n+1)-1 downto 0);
  
  SIGNAL inner_block_counter_update, mem_Wr_update    : STD_LOGIC:='0';
  SIGNAL col_block_counter_update                     : STD_LOGIC:='0';
  SIGNAL rInner_block_counter_update                  : STD_LOGIC_VECTOR(7 downto 0);
  SIGNAL rMem_Wr_update                               : STD_LOGIC_VECTOR(7 downto 0);
  SIGNAL Read_A, Read_B, rst_pe_c_sig                 : STD_LOGIC;  
  SIGNAL trig_rst_C_mm, trig_col_up                   : STD_LOGIC:='0';
  
begin

run_proc: process(clk)
  variable rRow_block_counter                : unsigned(counter_width-1 downto 0);
  variable rCol_block_counter                : unsigned(counter_width-1 downto 0);
begin
  if rising_edge(clk) then
    if rst='1' or run = '0' then
      run_state   <= halt;
      AB_val    <= '0';
      Read_A    <= '0';
      Read_B    <= '0';
      inner_block_counter    <= (others=>'0');
      row_block_counter      <= (others=>'0');
      col_block_counter      <= (others=>'0');
      max_innerblock_counter <= (others=>'0');
      max_rowblock_counter   <= (others=>'0');
      max_colblock_counter   <= (others=>'0');
      rowPart_n_counterA     <= (others=>'0');
      rowPart_n_counterB     <= (others=>'0');
      rowPart_n_counterC     <= (others=>'0');
      rowpart_n_counterc_reg <= (others=>'0');
      n_counter              <= (others=>'0');
      IA_Address             <= (others=>'0');
      IB_Address             <= (others=>'0');
      OC_Address             <= (others=>'0');
      OC_Remainder_col       <= (others=>'0');
      OC_Remainder_row       <= (others=>'0');
		OC_Remainder_inner     <= (others=>'0');
      inner_block_counter_update <= '0';
      col_block_counter_update   <= '0';
      mem_Wr_update              <= '0';
   else
   
     inner_block_counter_update  <= '0';
     col_block_counter_update  <= '0';
     mem_Wr_update          <= '0';
     rowpart_n_counterc_reg    <= rowpart_n_counterc;
    
     Case run_state is
      when halt =>
         if run_complete = '1' then -- Wait 1 more cycle in halt
            run_complete <= '0'; 
        else
          run_state  <= mem_rd;
          max_innerblock_counter  <= uceildiv(conf.A_innerSz,to_unsigned(n,counter_width)); --TODO: Optimize!
          max_rowblock_counter    <= uceildiv(conf.A_outerSz,to_unsigned(n,counter_width));
          max_colblock_counter    <= uceildiv(conf.B_outerSz,to_unsigned(n,counter_width));
          OC_Remainder_col      <= conf.B_outerSz;
          OC_Remainder_row      <= conf.A_outerSz;
			 OC_Remainder_inner	  <= conf.A_innerSz;
        end if;  
      when mem_rd  =>
        run_state  <= mem_rd_wait;
        n_counter  <= (others=>'0'); --mem_wr_wait is not long enough to reset n_counter
		  if inner_block_counter_update  = '1' then
		  	 OC_Remainder_inner   <= OC_Remainder_inner - n;
		  end if;
      when mem_rd_wait =>
        if (Empty_B='0' AND rowPart_n_counterB=0) 
           OR (Empty_A = '0' and Empty_B = '0')
               OR (Empty_A = '0' and  rowPart_n_counterB=n) then
          run_state <= exec;  
        end if;
      when exec =>
        n_counter  <= n_counter + 1;
        if n_counter = n-1 then
          n_counter          <= (others=>'0');
          
          rowPart_n_counterB  <= rowPart_n_counterB + 1;
          if rowPart_n_counterB > 0 then
            rowPart_n_counterA  <= rowPart_n_counterA + 1;
          end if;
          
          if rowPart_n_counterB = n then
             
            if inner_block_counter = max_innerblock_counter-1 then
              inner_block_counter  <= (others=>'0');
              if col_block_counter = max_colblock_counter-1 then
                 col_block_counter  <= (others=>'0');
                if col_block_counter /= 0 then
                   col_block_counter_update <= '1';
                end if;    
                if row_block_counter = max_rowblock_counter-1 then
                  row_block_counter  <= (others=>'0');
                  --DONE!
                else
                  row_block_counter  <= row_block_counter + 1;
                end if;
              else
                col_block_counter  <= col_block_counter + 1;
                col_block_counter_update <= '1';
              end if;
            else
              inner_block_counter  <= inner_block_counter + 1;
              inner_block_counter_update  <= '1';
            end if;
            
            rowPart_n_counterB  <= (others=>'0');
            rowPart_n_counterA  <= (others=>'0');
          end if;
          
          if inner_block_counter = max_innerblock_counter-1 and rowpart_n_counterb = n then
            run_state           <= mem_wr;
            mem_Wr_update       <= '1';
          else
            run_state  <= mem_rd;
          end if;
        end if;
        
       when mem_wr =>
         if Cout_val = '1' then
           run_state           <= mem_wr_wait;
           n_counter           <= n_counter + 1;
			  OC_Remainder_inner  <= conf.A_innerSz;
           if n_counter = n-2 then --Needed for n=2 only
             rowPart_n_counterC    <= rowPart_n_counterC + 1;
           end if;
        end if;

      when mem_wr_wait =>
        if n_counter = n-1 then
          n_counter  <= (others=>'0');
        else
           n_counter  <= n_counter + 1;        
        end if;  
        if n_counter = n-2 then --Breaking the design pattern: couting starts with mem_wr!
          if rowPart_n_counterC = n-1 then
            rowPart_n_counterC  <= (others=>'0');
            if inner_block_counter = 0 AND col_block_counter = 0 AND row_block_counter = 0 then
              run_state  <= halt;
              run_complete<= '1';
            else
              OC_Remainder_col   <= OC_Remainder_col - n;
              run_state          <= mem_rd;
              if col_block_counter = 0 then
                OC_Remainder_col <= conf.B_outerSz;
					 OC_Remainder_row <= OC_Remainder_row - n;
              end if;
            end if;
          else
            rowPart_n_counterC    <= rowPart_n_counterC + 1;
          end if;
        end if;
      when others =>
        run_state   <= halt;
    end Case;
    
    if run = '1' then --TODO: Loose control: Whenever data is available, just read!
      if Empty_A = '0' and ( rowPart_n_counterA = (n-1) OR (Empty_B = '0' and rowPart_n_counterB > 0)) then
        Read_A  <= '1';
      else
        Read_A  <= '0';  
      end if;
      if Empty_B = '0' and ( rowPart_n_counterB = 0 OR (Empty_A = '0' and rowPart_n_counterA < n-1)) then
        Read_B  <= '1';
      else
        Read_B  <= '0';        
      end if;
      AB_val  <= (Read_A and not(Empty_A)) OR (Read_B and not(Empty_B));
    end if;
    
    -- When writing, pipe is paused:
    -- No address change -> No fifo Data -> No MM operations
    if run = '1' AND run_state /= mem_wr AND run_state /= mem_wr_wait AND run_state /= halt then
      if rowPart_n_counterB > 0 then
        IA_Address  <= std_logic_vector(resize(unsigned(conf.A_Address) 
              + rowPart_n_counterA    *conf.A_outerSz
              + inner_block_counter  *conf.A_outerSz   *n
              + row_block_counter    *n, awidth));
      end if;        
      if rowPart_n_counterB < n then
        IB_Address  <= std_logic_vector(resize(unsigned(conf.B_Address)
              + rowPart_n_counterB    *conf.B_outerSz
              + col_block_counter    *n
              + inner_block_counter  *conf.B_outerSz  *n, awidth));  
      end if;
     end if;  
     
     -- When writing, OC_Address is changed N times
     if run = '1' AND ((run_state = mem_wr AND rMem_Wr_update(1) = '1' ) OR run_state = mem_wr_wait) then
       if max_colblock_counter > 1 then
        rCol_block_counter := ternu(col_block_counter=0,max_colblock_counter-1,col_block_counter-1);
         rRow_block_counter := ternu(col_block_counter=0,ternu(row_block_counter=0,max_rowblock_counter-1,row_block_counter-1),row_block_counter);
      else
        rCol_block_counter := col_block_counter;
         rRow_block_counter := row_block_counter;
      end if;
      
      if cntIn.comm.operation = OPR_MMT then
        OC_Address  <= std_logic_vector(resize(unsigned(conf.C_Address)
                + rCol_block_counter    *n
                + rRow_block_counter    *conf.B_outerSz   *n
                + rowPart_n_counterC    *conf.B_outerSz, awidth));
      else
        OC_Address  <= std_logic_vector(resize(unsigned(conf.C_Address)
                + rCol_block_counter    *conf.A_outerSz   *n
                + rRow_block_counter    *n
                + rowPart_n_counterC    *conf.A_outerSz, awidth));
      end if;        
     end if;  
     
   end if;
  end if;
end process;

rst_pe_c_proc: process(clk)
begin
  if rising_edge(clk) then
    if rst='1' or run = '0' then
     trig_rst_C_mm      <= '0';
     trig_col_up      <= '0';
   else
      trig_rst_C_mm      <= '0';
      if col_block_counter_update = '1' then
       trig_col_up  <= '1';
     end if;
     if   trig_col_up = '1' and 
        run_state = mem_wr_wait and
        rowpart_n_counterc_reg = n-1 and
        n_counter = n-2 then
      trig_rst_C_mm  <= '1';
      trig_col_up  <= '0';
    end if;  
   end if;
  end if;
end process;

rst_pe      <= rInner_block_counter_update(0) OR rst_pe_c_sig;
rst_pe_c_sig  <= trig_rst_C_mm OR run_complete;
rst_pe_c      <= rst_pe_c_sig;
run_complete_o  <= run_complete;
Read_A_o        <= Read_A;
Read_B_o        <= Read_B;
  
shift_memwru: entity matrix_mul_v1_00_a.shiftreg 
              generic map(8) port map(clk,mem_Wr_update,rMem_Wr_update);
shift_ibcu: entity matrix_mul_v1_00_a.shiftreg 
              generic map(8) port map(clk,inner_block_counter_update,rInner_block_counter_update);

conf     <= cntIn.conf;    
Cout_val <= cntIn.Cout_val;
Empty_A  <= cntIn.Empty_A; 
Empty_B  <= cntIn.Empty_B; 

OCC_count  <= std_logic_vector(umin(to_unsigned(n,counter_width), OC_Remainder_col));
OCR_count  <= std_logic_vector(umin(to_unsigned(n,counter_width), OC_Remainder_row));
AB_count   <= std_logic_vector(umin(to_unsigned(n,counter_width), OC_Remainder_inner));

cntOut.A_Data       <= (others=>'0');
cntOut.B_Data       <= (others=>'0');
cntOut.AB_val       <= AB_val;        
cntOut.Read_A       <= Read_A_o;      
cntOut.Read_B       <= Read_B_o;
cntOut.rst_pe       <= rst_pe;         
cntOut.rst_pe_c     <= rst_pe_c;       
cntOut.run_complete <= run_complete_o;
cntOut.IA_Address   <= IA_Address;
cntOut.IA_Burst     <= std_logic_vector(to_unsigned(n,counter_width));
cntOut.IB_Address   <= IB_Address;     
cntOut.IB_Burst     <= std_logic_vector(to_unsigned(n,counter_width));
cntOut.OC_Address   <= OC_Address;
cntOut.OCC_count    <= OCC_count;
cntOut.OCR_count    <= OCR_count;
cntOut.AB_count     <= AB_count;

end Behavioral;


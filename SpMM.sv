/* verilator lint_off WIDTHEXPAND */
/* verilator lint_off WIDTHTRUNC */
/* verilator lint_off CMPCONST */
/* verilator lint_off UNSIGNED */
/* verilator lint_off UNOPTFLAT */
`ifndef N
`define N              16
`endif
`define W               8
`define lgN     ($clog2(`N))
`define dbLgN (2*$clog2(`N))

typedef struct packed { logic [`W-1:0] data; } data_t;

module add_(
    input   logic   clock,
    input   data_t  a,
    input   data_t  b,
    output  data_t  out
);
    always_ff @(posedge clock) begin
        out.data <= a.data + b.data;
    end
endmodule

module mul_(
    input   logic   clock,
    input   data_t  a,
    input   data_t  b,
    output  data_t out
);
    always_ff @(posedge clock) begin
        out.data <= a.data * b.data;
    end
endmodule

module RedUnit(
    input   logic               clock,
                                reset,
    input   data_t              data[`N-1:0],
    input   logic               split[`N-1:0],
    input   logic [`lgN-1:0]    out_idx[`N-1:0],
    input   data_t              halo_in,
    input   logic               line_count[`N-1:0],
    output  data_t              out_data[`N-1:0],
    output  data_t              halo_out,
    output  int                 delay,
    output  int                 num_el
);
    // num_el 总是赋值为 N
    assign num_el = `N;
    // delay 你需要自己为其赋值，表示电路的延迟
    assign delay = `dbLgN;
    data_t pfxsum_pass [`lgN:0][`N-1:0];
    data_t pfxsum[`lgN:0][`N-1:0];
    logic  [`lgN-1:0] split_map[`lgN:0][`N-1:0];
    logic  [`lgN-1:0] split_reg[`lgN:0][`N-1:0];
    logic  [`lgN-1:0] out_idx_pass[`lgN:0][`N-1:0];
    logic  [`lgN-1:0] out_idx_reg[`lgN:0][`N-1:0];
    logic  [`lgN-1:0] shift_idx[`lgN:0];
    logic  halo_switch[`lgN:0];
    data_t zero;
    //Fetch inputs
    assign zero.data = 0;
    assign out_idx_pass[0] = out_idx;  
    assign pfxsum_pass[0] = data;
    generate
        for (genvar i = 0; i < `N; i++) begin
            assign split_map[0][i] = split[i];
        end
    endgenerate
    //Split Prefix Sum: Segmentation
    generate 
        for (genvar i = 1; i <= `lgN; i++) begin
            assign shift_idx[i] = 1 << (i-1);
            for (genvar j = 0; j < `N; j++) begin
                always_ff @(posedge clock) begin
                    split_map[i][j] <= split_map[i-1][j] + (j<shift_idx[i]? 0 : split_map[i-1][j-shift_idx[i]]);
                end
                always_ff @(posedge clock) begin
                    pfxsum_pass[i][j] <= pfxsum_pass[i-1][j];
                    out_idx_pass[i][j] <= out_idx_pass[i-1][j];
                end
            end 
        end
    endgenerate
    //Passing Data
    generate
        for (genvar i = 1; i < `N; i++) begin
            assign split_reg[0][i] = split_map[`lgN][i-1];
        end
    endgenerate
    assign halo_switch[0] = (split_map[`lgN][`N-1] == split_map[`lgN][`N-2]); //halo on
    assign split_reg[0][0] = 0;
    assign pfxsum[0] = pfxsum_pass[`lgN];
    assign out_idx_reg[0] = out_idx_pass[`lgN];
    //Prefix Sum: Accumulation
    generate 
        for (genvar i = 1; i <= `lgN; i++) begin
            for (genvar j = 0; j < `N; j++) begin
                add_ adder_unit(
                    .clock(clock),
                    .a(pfxsum[i-1][j]),
                    .b(j<shift_idx[i]? zero : (split_reg[i-1][j]!=split_reg[i-1][j-shift_idx[i]]? zero : pfxsum[i-1][j-shift_idx[i]])),
                    .out(pfxsum[i][j])
                );
                always_ff @(posedge clock) begin
                    split_reg[i][j] <= split_reg[i-1][j];
                    out_idx_reg[i][j] <= out_idx_reg[i-1][j];
                    halo_switch[i] <= halo_switch[i-1];
                end
            end 
        end
    endgenerate

    assign out_data[0] = pfxsum[`lgN][out_idx_reg[`lgN][0]];
    generate 
        for (genvar i = 1; i < `N; i++) begin
            assign out_data[i].data = pfxsum[`lgN][out_idx_reg[`lgN][i]].data + ((line_count[i]==0 && line_count[i-1]==1)? halo_in.data : 0);
        end
    endgenerate
    assign halo_out = halo_switch[`lgN]? pfxsum[`lgN][`N-1] : zero;
    
endmodule


module RedUnit_Addertree(
    input   logic               clock,
                                reset,
    input   data_t              data[`N-1:0],
    input   logic               split[`N-1:0],
    input   logic [`lgN-1:0]    out_idx[`N-1:0],
    output  data_t              out_data[`N-1:0],
    output  int                 delay,
    output  int                 num_el
);
    // num_el 总是赋值为 N
    assign num_el = `N;
    // delay 你需要自己为其赋值，表示电路的延迟
    assign delay = `lgN;

    data_t addertree[`lgN:0][`N-1:0];
    assign addertree[0] = data;
    generate
        for (genvar i = 0; i < `lgN; i++) begin
            for (genvar j = 0; j < (`N >> (i+1)); j++) begin
                add_ adder_unit(
                    .clock(clock),
                    .a(addertree[i][j*2]),
                    .b(addertree[i][j*2+1]),
                    .out(addertree[i+1][j])
                );
            end
        end
    endgenerate

    generate
        for (genvar i = 0; i < `N; i++) begin
            assign out_data[i] = addertree[`lgN][`N-i-1];
        end
    endgenerate

endmodule

module PE(
    input   logic               clock,
                                reset,
    input   logic               lhs_start,
    input   logic [`dbLgN-1:0]  lhs_ptr [`N-1:0],
    input   logic [`lgN-1:0]    lhs_col [`N-1:0],
    input   data_t              lhs_data[`N-1:0],
    input   data_t              rhs[`N-1:0],
    output  data_t              out[`N-1:0],
    output  logic               line_valid[`N-1:0],
    output  int                 delay,
    output  int                 num_el
);
    // num_el 总是赋值为 N
    assign num_el = `N;
    // delay 你需要自己为其赋值，表示电路的延迟
    assign delay = `dbLgN + 1;
    data_t xbar[`N-1:0];
    data_t data[`N-1:0];
    logic  split[1:0][`N-1:0];
    logic  [`lgN-1:0] out_idx[`N-1:0];
    logic  [`dbLgN-1:0] ptr[`N-1:0];
    logic  [`lgN:0] lhs_cycle;
    logic  empty_line[`dbLgN:0][`N-1:0];
    logic  line_count[`dbLgN:0][`N-1:0];
    data_t out_redunit[`N-1:0];
    data_t halo_in = '{data: 0};
    data_t halo_out;
    data_t zero;
    logic  split_switch;
    int    delay_redunit;
    //lhs_ptr keeping
    assign zero.data = 0;
    generate
        for (genvar i = 0; i < `N; i++) begin
            always_ff @(posedge clock) begin
                if (lhs_start) begin
                    ptr[i] <= lhs_ptr[i];
                end
            end
        end
    endgenerate
    //lhs_cycle counting
    always_ff @(posedge clock) begin
        if (lhs_start) begin
            lhs_cycle <= 0;
        end
        else if (lhs_cycle == `N) begin
            lhs_cycle <= 0;
        end
        else begin
            lhs_cycle <= lhs_cycle + 1;
        end
    end
    assign split_switch = lhs_cycle % 2;
    //split generate
    generate
        for (genvar i = 0; i < `N; i++) begin
            assign split[split_switch][ptr[i]%`N] = split[split_switch][ptr[i]%`N] || (ptr[i]-lhs_cycle*`N >=0 && ptr[i]-lhs_cycle*`N < `N);
            always_ff @(posedge clock) begin
                split[!split_switch][i] <= 0;
            end
        end
    endgenerate
    //out_idx generate
    assign empty_line[0][0] = 0;
    assign out_idx[0] = ptr[0] % `N;
    generate
        for (genvar i = 1; i < `N; i++) begin
            assign out_idx[i] = ptr[i] % `N;
            assign empty_line[0][i] = (ptr[i] == ptr[i-1]);//put 0 to the empty line
        end
    endgenerate
    generate
        for (genvar i = 1; i <= `dbLgN; i++) begin
            always_ff @(posedge clock) begin
                empty_line[i] <= empty_line[i-1];
            end
        end
    endgenerate
    //X-bar generate
    generate
        for (genvar i = 0; i < `N; i++) begin
            assign xbar[i].data = rhs[lhs_col[i]].data;
        end
    endgenerate
    //Inner Product
    generate
        for(genvar i = 0; i < `N; i++) begin
            mul_ mul_unit(
                .clock(clock),
                .a(lhs_data[i]),
                .b(xbar[i]),
                .out(data[i])
            );
        end
    endgenerate
    //find out the lines we have calculated
    generate
        for (genvar i = 0; i < `N; i++) begin
            assign line_count[0][i] = (lhs_cycle==0)? 0 : ((lhs_cycle)*`N-1 >= ptr[i]? 1 : 0);
        end
    endgenerate
    generate
        for (genvar i = 1; i <= `dbLgN; i++) begin
            always_ff @(posedge clock) begin
                line_count[i] <= line_count[i-1];
            end
        end
    endgenerate
    generate
        for (genvar i = 0; i < `N; i++) begin
            assign line_valid[i] = line_count[`dbLgN-1][i] - line_count[`dbLgN][i];
        end
    endgenerate
    //Call RedUnit
    RedUnit red_unit(
        .clock(clock),
        .reset(reset),
        .data(data),
        .split(split[split_switch]),
        .out_idx(out_idx),
        .halo_in(halo_in),
        .line_count(line_count[`dbLgN]),
        .out_data(out_redunit),
        .halo_out(halo_out),
        .delay(delay_redunit),
        .num_el(num_el)
    );
    //Halo Ctrl
    always_ff @(posedge clock) begin
        if (lhs_start) begin
            halo_in.data <= 0;
        end
        else begin
            halo_in <= halo_out;
        end
    end
    //PE OUTPUT
    assign out = out_redunit;
    generate
        for (genvar i = 0; i < `N; i++) begin
            assign out[i] = empty_line[`dbLgN][i]? zero : out_redunit[i]; //put 0 on the empty line
        end
    endgenerate
endmodule

module SpMM(
    input   logic               clock,
                                reset,
    /* 输入在各种情况下是否 ready */
    output  logic               lhs_ready_ns,
                                lhs_ready_ws,
                                lhs_ready_os,
                                lhs_ready_wos,
    input   logic               lhs_start,
    /* 如果是 weight-stationary, 这次使用的 rhs 将保留到下一次 */
                                lhs_ws,
    /* 如果是 output-stationary, 将这次的结果加到上次的 output 里 */
                                lhs_os,
    input   logic [`dbLgN-1:0]  lhs_ptr [`N-1:0],
    input   logic [`lgN-1:0]    lhs_col [`N-1:0],
    input   data_t              lhs_data[`N-1:0],
    output  logic               rhs_ready,
    input   logic               rhs_start,
    input   data_t              rhs_data [3:0][`N-1:0],
    output  logic               out_ready,
    input   logic               out_start,
    output  data_t              out_data [3:0][`N-1:0],
    output  int                 num_el
);
    // num_el 总是赋值为 N
    assign num_el = `N;
    assign lhs_ready_ns = 0;
    assign lhs_ready_ws = 0;
    assign lhs_ready_os = 0;
    assign lhs_ready_wos = 0;
    assign out_ready = 0;
    int delay_pe;
    assign delay_pe = `dbLgN + 1;
// RHS INPUT CTRL
    data_t rhs_buffer[`N-1:0][`N-1:0];
    logic [1:0] rhs_state;
    logic [`lgN-1:0] rhs_idx;
    //logic rhs_buffer_state [1:0];
    logic [1:0] rhs_buffer_state;
    logic buffer_choose;
    localparam RHS_EMPTY = 0;
    localparam RHS_BUSY = 1;
    localparam RHS_FILLED = 2;
    always_ff @(posedge clock) begin
        if (reset || rhs_idx == `N/4-1) begin
            rhs_idx <= 0;
        end
        else if (rhs_start || rhs_state == RHS_BUSY) begin
            rhs_idx <= rhs_idx + 1;
        end
        else rhs_idx <= 0;
    end
    always_ff @(posedge clock) begin
        if (reset) begin
            rhs_state <= RHS_EMPTY;
        end
        else case(rhs_state)
            RHS_EMPTY: begin
                if (rhs_start) begin
                    rhs_state <= RHS_BUSY;
                end
            end
            RHS_BUSY: begin
                if (rhs_idx == `N/4-1 && (rhs_start || rhs_state == RHS_BUSY)) begin
                    rhs_state <= RHS_FILLED;
                end
                else rhs_state <= RHS_BUSY;
            end
            RHS_FILLED: begin
                rhs_state <= RHS_EMPTY;
            end
        endcase
    end
//RHS BUFFER READ
    always_ff @(posedge clock) begin
        if (reset) begin
            //rhs_buffer_state[0] <= RHS_EMPTY;
            //rhs_buffer_state[1] <= RHS_EMPTY;
            //buffer_choose <= 0;
            rhs_buffer_state <= RHS_EMPTY;
             for (int j = 0; j < `N; j++) begin
                for (int k = 0; k < `N; k++) begin
                    rhs_buffer[j][k].data <= 0;
                end
            end
        end
        else if (rhs_start || rhs_state == RHS_BUSY) begin
            rhs_buffer_state <= RHS_BUSY;
            for (int i = 0; i < 4; i++) begin
                for (int j = 0; j < `N; j++) begin
                    rhs_buffer[j][i + 4*rhs_idx].data <= rhs_data[i][j].data;
                end
            end
        end
        else if (rhs_state == RHS_FILLED) begin
            rhs_buffer_state <= RHS_FILLED;
        end
    end
    assign rhs_ready = (rhs_state == RHS_EMPTY);

//LHS INPUT CTRL
    logic [1:0] lhs_state;
    logic [`lgN-1:0] lhs_idx;  //counting lhs input cycles
    logic [`dbLgN-1:0] output_temp_delay; //waiting for PE delay: this marks the begin of the PE output
    localparam LHS_EMPTY = 0; 
    localparam LHS_BUSY = 1;
    localparam LHS_FILLED = 2;
    always_ff @(posedge clock) begin
        if (reset) begin
            lhs_idx <= 0;
        end
        else if (lhs_start || lhs_state == LHS_BUSY) begin
            lhs_idx <= lhs_idx + 1;
        end
        else lhs_idx <= 0;
    end
    always_ff @(posedge clock) begin
        if (reset || output_temp_delay == delay_pe) begin
            output_temp_delay <= 0;
        end
        else if (lhs_start || output_temp_delay!=0) begin
            output_temp_delay <= output_temp_delay + 1;
        end
    end
    always_ff @(posedge clock) begin
        if (reset || rhs_state == RHS_FILLED) begin
            lhs_state <= LHS_EMPTY;
        end
        else if (lhs_start) begin
            lhs_state <= LHS_BUSY;
        end
        else if (lhs_state == LHS_BUSY && (lhs_idx+1)*`N-1 >= lhs_ptr[`N-1]) begin
            lhs_state <= LHS_FILLED;
            rhs_buffer_state <= RHS_EMPTY;
        end
        else if (lhs_state == LHS_FILLED) begin
            lhs_state <= LHS_EMPTY;
        end
    end
    assign lhs_ready_ns = (lhs_state == LHS_EMPTY && rhs_buffer_state == RHS_FILLED);

// OUTPUT CTRL
    logic [`dbLgN-1:0] out_state;   //waiting for PE delay: this marks the begin of the output_ready
    logic [`lgN-1:0] out_line_idx;  //find out the output line index
    always_ff @(posedge clock) begin
        if (reset || out_state == delay_pe) begin
            out_state <= 0;
        end
        else if ((lhs_state == LHS_BUSY && (lhs_idx+1)*`N-1 >= lhs_ptr[`N-1]) || out_state != 0) begin
            out_state <= out_state + 1;
        end
    end
    always_ff @(posedge clock) begin
        if (reset || out_line_idx == `N-1) begin
            out_line_idx <= 0;
        end
        else if (output_temp_delay == delay_pe || out_line_idx != 0) begin
            out_line_idx <= out_line_idx + 1;
        end
    end
    always_ff @(posedge clock) begin
        if (reset) begin
            out_ready <= 0;
        end
        else if (out_line_idx == `N-1) begin //modified for test!!! original: out_line_state == `N-1
            out_ready <= 1;
        end
        else begin
            out_ready <= 0;
        end
    end
    //assign out_ready = (out_state == delay_pe);

//OUTPUT BUFFER CTRL
    data_t out_buffer[`N-1:0][`N-1:0];
    data_t shift_temp[`N-1:0];
    data_t out_temp[`N-1:0][`N-1:0];
    logic [`lgN-1:0] out_buf_idx;
    logic [1:0] out_buffer_state;
    localparam OUTPUT_BUF_EMPTY = 0;
    localparam OUTPUT_BUF_BUSY = 1;
    localparam OUTPUT_BUF_FILLED = 2;
    always_ff @(posedge clock) begin
        if (reset) begin
            out_buffer_state <= OUTPUT_BUF_EMPTY;
        end
        else if (lhs_start || (lhs_state == LHS_BUSY)) begin
            out_buffer_state <= OUTPUT_BUF_BUSY;
        end
        else if (out_line_idx == `N-2) begin
            out_buffer_state <= OUTPUT_BUF_FILLED;
        end
    end
    always_ff @(posedge clock) begin
        if (reset || out_buf_idx == `N/4) begin
            out_buffer_state <= OUTPUT_BUF_EMPTY;
            out_buf_idx <= 0;
        end
        else if (out_start || out_buffer_state == OUTPUT_BUF_FILLED) begin
            out_buf_idx <= out_buf_idx + 1;
        end
    end
// Shift Register
    generate
        for (genvar i = 0; i < `N; i++) begin
            for (genvar j = 0; j < `N; j++) begin
                always_ff @(posedge clock) begin
                    if (reset) begin
                        out_buffer[i][j].data <= 0;
                    end
                    else if (out_buffer_state == OUTPUT_BUF_BUSY) begin
                        if (i == `N-1) begin
                            out_buffer[i][j].data <= 0;
                        end
                        else begin
                            out_buffer[i][j].data <= out_buffer[i+1][j].data;
                        end
                    end
                end
            end
        end
    endgenerate
//PE
    int delay;
    logic line_valid[`N-1:0];
    data_t out_valid[`N-1:0][`N-1:0];
    generate
        for (genvar i = 0; i < `N; i++) begin
            PE pe_unit(
                .clock(clock),
                .reset(reset),
                .lhs_start(lhs_start),
                .lhs_ptr(lhs_ptr),
                .lhs_col(lhs_col),
                .lhs_data(lhs_data),
                .rhs(rhs_buffer[i]),
                .out(out_temp[i]),
                .line_valid(line_valid),
                .delay(delay),
                .num_el(num_el)
            );
            for (genvar j = 0; j < `N; j++) begin
               assign out_valid[i][j] = line_valid[j]? out_temp[i][j] : out_valid[i][j];
            end
            assign out_buffer[`N-1][i].data = (out_line_idx==0 && out_buffer_state!=OUTPUT_BUF_BUSY)? out_buffer[`N-1][i].data : out_valid[i][out_line_idx].data; //block unwant results
        end
    endgenerate

//OUTPUT
    always_ff @(posedge clock) begin
        if (reset) begin
            for (int i = 0; i < 4; i++) begin
                for (int j = 0; j < `N; j++) begin
                    out_data[i][j] <= 0;
                end
            end
        end
        else if (out_line_idx == `N-2 || out_buffer_state == OUTPUT_BUF_FILLED) begin //modified for test!!! original: out_line_state == `N-2
            for (int i = 0; i < 4; i++) begin
                for (int j = 0; j < `N; j++) begin
                    out_data[i][j].data <= out_buffer[i+out_buf_idx*4][j].data;
                    //out_data[i][j].data <= out_buffer[i+4][j].data;
                end
            end
        end
        else begin
            for (int i = 0; i < 4; i++) begin
                for (int j = 0; j < `N; j++) begin
                    out_data[i][j].data <= 0;
                end
            end
        end
    end
endmodule
/* verilator lint_off WIDTHEXPAND */
/* verilator lint_off WIDTHTRUNC */
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

module RedUnit_Pfxsum(
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
    data_t pfxsum [`lgN:0][`N-1:0];
    logic [`lgN-1:0] shift_idx;
    data_t zero;
    assign zero.data = 0;
    assign pfxsum[0] = data;
    generate 
        for (genvar i = 1; i <= `lgN; i++) begin
            assign shift_idx = 1 << (i-1);
            for (genvar j = 0; j < `N; j++) begin
                add_ adder_unit(
                    .clock(clock),
                    .a(pfxsum[i-1][j]),
                    .b(j<shift_idx? zero : pfxsum[i-1][j-shift_idx]),
                    .out(pfxsum[i][j])
                );
            end
        end
    endgenerate
    generate
        for (genvar i = 0; i < `N; i++) begin
            assign out_data[i] = pfxsum[`lgN][i];
        end
    endgenerate
    /*
    generate
        for (genvar i = 0; i < `N; i++) begin
            add_ minusunit(
                .clock(clock),
                .a(pfxsum[`lgN][out_idx[i]]),
                .b(i==0? zero : -pfxsum[`lgN][out_idx[i-1]]),
                .out(out_data[i])
            );
        end
    endgenerate
    */
endmodule


module RedUnit(
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
    output  int                 delay,
    output  int                 num_el
);
    // num_el 总是赋值为 N
    assign num_el = `N;
    // delay 你需要自己为其赋值，表示电路的延迟
    assign delay = `lgN + 1;
    data_t data[`N-1:0];
    logic split[`N-1:0];
    logic [`lgN-1:0] out_idx[`N-1:0];
    data_t out_redunit[`N-1:0];
    int delay_redunit;
    generate
        for(genvar i = 0; i < `N; i++) begin
            mul_ mul_unit(
                .clock(clock),
                .a(lhs_data[i]),
                .b(rhs[i]),
                .out(data[i])
            );
        end
    endgenerate
    RedUnit red_unit(
        .clock(clock),
        .reset(reset),
        .data(data),
        .split(split),
        .out_idx(out_idx),
        .out_data(out_redunit),
        .delay(delay_redunit),
        .num_el(num_el)
    );
    generate
        for(genvar i = 0; i < `N; i++) begin
            assign out[i] = out_redunit[`N-1];
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
    assign delay_pe = `lgN + 1;
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
    logic [`lgN-1:0] lhs_idx;   
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
        if (reset || rhs_state == RHS_FILLED) begin
            lhs_state <= LHS_EMPTY;
        end
        else if (lhs_start) begin
            lhs_state <= LHS_BUSY;
        end
        else if (lhs_state == LHS_BUSY && lhs_idx == `N-1) begin
            lhs_state <= LHS_FILLED;
            rhs_buffer_state <= RHS_EMPTY;
        end
        else if (lhs_state == LHS_FILLED) begin
            lhs_state <= LHS_EMPTY;
        end
    end
    assign lhs_ready_ns = (lhs_state == LHS_EMPTY && rhs_buffer_state == RHS_FILLED);

// PE CTRL
    logic pe_lhs_start_signal[1:0];
    logic pe_lhs_start;
    always_ff @(posedge clock) begin
        if (reset) begin
            pe_lhs_start_signal[1] <= 0;
        end
        else begin
            if ((lhs_idx+1)%4 == 0 && lhs_idx!=`N-1) begin
                pe_lhs_start_signal[1] <= 1;
            end
            else begin
                pe_lhs_start_signal[1] <= 0;
            end
        end
    end
    assign pe_lhs_start_signal[0] = lhs_start;
    assign pe_lhs_start = pe_lhs_start_signal[0] + pe_lhs_start_signal[1];

// OUTPUT CTRL
    logic [`lgN-1:0] out_state;
    
    always_ff @(posedge clock) begin
        if (reset || out_state == delay_pe) begin
            out_state <= 0;
        end
        else if (lhs_idx == `N-1 || out_state != 0) begin
            out_state <= out_state + 1;
        end
    end
    always_ff @(posedge clock) begin
        if (reset) begin
            out_ready <= 0;
        end
        else if(out_state == delay_pe) begin
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
        else if (out_state == delay_pe-1) begin
            out_buffer_state <= OUTPUT_BUF_FILLED;
        end
    end
    always_ff @(posedge clock) begin
        if (reset) begin
            out_buf_idx <= 0;
        end
        else if (out_buf_idx == `N/4) begin
            out_buffer_state <= OUTPUT_BUF_EMPTY;
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
                            out_buffer[i][j].data <= out_buffer[0][j].data;
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
    generate
        for (genvar i = 0; i < `N; i++) begin
            PE pe_unit(
                .clock(clock),
                .reset(reset),
                .lhs_start(pe_lhs_start),
                .lhs_ptr(lhs_ptr),
                .lhs_col(lhs_col),
                .lhs_data(lhs_data),
                .rhs(rhs_buffer[i]),
                .out(out_temp[i]),
                .delay(delay),
                .num_el(num_el)
            );
            assign out_buffer[`N-1][i].data = out_temp[i][0].data;
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
        else if (out_state == delay_pe-1 || out_buffer_state == OUTPUT_BUF_FILLED) begin
            for (int i = 0; i < 4; i++) begin
                for (int j = 0; j < `N; j++) begin
                    out_data[i][j].data <= out_buffer[i+out_buf_idx*4][j].data;
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

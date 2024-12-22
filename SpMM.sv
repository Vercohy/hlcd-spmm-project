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
module num_(
    input logic clock,
    input logic num_in,
    output logic num_out
);
    always_ff  @(posedge clock) begin
        num_out <= num_in /2;
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
    assign rhs_ready = 0;
    assign out_ready = 0;
endmodule

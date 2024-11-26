#include "gen_code.h"
#include "ast.h"
#include "code_seq.h"
#include "code_utils.h"
#include "bof.h"
#include "literal_table.h"
#include "instruction.h"
#include "regname.h"
#include "utilities.h"
#include <string.h>
#include "spl.tab.h"
#include "code.h"

//generates code for a program by using asts and turning them into BOF files


#define STACK_SPACE 4096

//initializes literal table
void gen_code_initialize(){
    literal_table_initialize();
}

//writes all instructions in order
static void gen_code_output_seq(BOFFILE bf, code_seq cs){
    while (!code_seq_is_empty(cs)){
      	bin_instr_t inst = code_seq_first(cs)->instr;
      	instruction_write_bin_instr(bf, inst);
      	cs = code_seq_rest(cs);
    }
}

//generates BOF header
BOFHeader gen_code_program_header(code_seq main_cs){
    BOFHeader ret;
    bof_write_magic_to_header(&ret);
    ret.text_start_address = 0;
    ret.text_length = code_seq_size(main_cs);
    ret.data_start_address = MAX(ret.text_length, 1024);
    ret.data_length = literal_table_size();
    ret.stack_bottom_addr = ret.data_start_address + ret.data_start_address + ret.data_length + STACK_SPACE;
    return ret;
}

//outputs just the literals
void gen_code_output_literals(BOFFILE bf){
    literal_table_start_iteration();
    while (literal_table_iteration_has_next()){
        word_type literal = literal_table_iteration_next();
        bof_write_word(bf, literal);
    }
    literal_table_end_iteration();
}

//outputs to a BOF file
void gen_code_output_program(BOFFILE bf, code_seq main_cs){
    BOFHeader header = gen_code_program_header(main_cs);
    bof_write_header(bf, header);
    gen_code_output_seq(bf, main_cs);
    gen_code_output_literals(bf);
    bof_close(bf);
}

//generates the entire program
void gen_code_program(BOFFILE bf, block_t prog){
    code_seq main_cs = gen_code_block(prog); 
    code_seq_concat(&main_cs, code_utils_tear_down_program());
    gen_code_output_program(bf, main_cs);
}

//block statement
code_seq gen_code_block(block_t block){
    code_seq ret = code_seq_empty(); 
    int total_stack_space = 0;
    
    if (block.var_decls.var_decls!=NULL){
        code_seq var_decls_cs = gen_code_var_decls(block.var_decls);
        int vars_len_in_bytes = code_seq_size(var_decls_cs);
        total_stack_space += vars_len_in_bytes;
        code_seq_concat(&ret, var_decls_cs);
    }
    
    if (block.const_decls.start!=NULL){
        code_seq const_decls_cs = gen_code_const_decls(block.const_decls);
        int const_len_in_bytes = code_seq_size(const_decls_cs);
        total_stack_space += const_len_in_bytes;
        code_seq_concat(&ret, const_decls_cs);
    }
    
    code_seq_concat(&ret, code_utils_set_up_program());
    
    if (block.stmts.stmts_kind!=empty_stmts_e){
        code_seq stmts_cs = gen_code_stmts(block.stmts);
        code_seq_concat(&ret, stmts_cs);
    }
    
    if (total_stack_space>0) code_seq_concat(&ret, code_utils_deallocate_stack_space(total_stack_space/2)); 
    
    return ret;
}

//multiple constant declarations
code_seq gen_code_const_decls(const_decls_t const_decls){
    code_seq ret = code_seq_empty();
    const_decl_t *cd_listp = const_decls.start;

    while (cd_listp != NULL){
        code_seq const_decl_cs = gen_code_const_decl(*cd_listp);
        code_seq_concat(&ret, const_decl_cs);
        cd_listp = cd_listp->next;
    }

    return ret;
}

//single constant declaration
code_seq gen_code_const_decl(const_decl_t const_decl){
    code_seq ret = code_seq_empty();

    code_seq const_def_list_cs = gen_code_const_def_list(const_decl.const_def_list);
    code_seq_concat(&ret, const_def_list_cs);

    return ret;
}

//list of constant definitions
code_seq gen_code_const_def_list(const_def_list_t const_def_list){
    code_seq ret = code_seq_empty();
    const_def_t *cdp = const_def_list.start;

    while (cdp != NULL){
        code_seq const_def_cs = gen_code_const_def(*cdp);
        code_seq_concat(&ret, const_def_cs);
        cdp = cdp->next;
    }

    return ret;
}

//single constant defintion
code_seq gen_code_const_def(const_def_t const_def){
    unsigned int offset = literal_table_lookup(const_def.number.text, const_def.number.value); // find offset
    code_seq ret = code_utils_allocate_stack_space(1);
    code_seq_add_to_end(&ret, code_cpw(SP, 0, GP, offset)); // Copy constant from literal table (GP + offset)

    return ret;

}


//car declarations
code_seq gen_code_var_decls(var_decls_t var_decls){
    code_seq ret = code_seq_empty();
    var_decl_t *vdp = var_decls.var_decls;

    while (vdp != NULL){
        code_seq var_decl_cs = gen_code_var_decl(*vdp);
        code_seq_concat(&var_decl_cs, ret);
        vdp = vdp->next;
    }
        
    return ret;
}

//single var declaration
code_seq gen_code_var_decl(var_decl_t var_decl){
    code_seq ret = gen_code_idents(var_decl.ident_list);
    return ret;
}

//identifiers
code_seq gen_code_idents(ident_list_t idents){
    code_seq ret = code_seq_empty();
    ident_t *idp = idents.start;

    while (idp != NULL){
        code_seq alloc_space = code_utils_allocate_stack_space(1);
        code_seq_concat(&ret, alloc_space);

        code_seq zero_init = code_seq_singleton(code_lit(SP, 0, 0));
        code_seq_add_to_end(&zero_init, code_swr(SP, 0, 3));
        code_seq_concat(&ret, zero_init);

        idp = idp->next;

    }
    
    return ret;
}

//statement lists
code_seq gen_code_stmts(stmts_t stmts){
    code_seq ret = code_seq_empty();

    stmt_list_t stmt_list = stmts.stmt_list;
    code_seq stmt_list_cs = gen_code_stmt_list(stmt_list);
    code_seq_concat(&ret, stmt_list_cs);

    return ret;
}

//statement list
code_seq gen_code_stmt_list(stmt_list_t stmt_list){
    code_seq ret = code_seq_empty();
    stmt_t *stmt = stmt_list.start;
    
    while (stmt != NULL){
        code_seq stmt_cs = gen_code_stmt(*stmt); 
        code_seq_concat(&ret, stmt_cs);
        stmt = stmt->next;
    }
    return ret;
}

//single statement
code_seq gen_code_stmt(stmt_t stmt){
    switch (stmt.stmt_kind){
        case assign_stmt:
            return gen_code_assign_stmt(stmt.data.assign_stmt);
        case call_stmt:
            return gen_code_call_stmt(stmt.data.call_stmt);
        case if_stmt:
            return gen_code_if_stmt(stmt.data.if_stmt);
        case while_stmt:
            return gen_code_while_stmt(stmt.data.while_stmt);
        case read_stmt:
            return gen_code_read_stmt(stmt.data.read_stmt);
        case print_stmt:
            return gen_code_print_stmt(stmt.data.print_stmt);
        case block_stmt:
            return gen_code_block_stmt(stmt.data.block_stmt);
        default:
            bail_with_error("error: undetectable statement type: %d", stmt.stmt_kind);
    }
    return code_seq_empty();
}

//assignment statement
code_seq gen_code_assign_stmt(assign_stmt_t stmt){
    code_seq ret = gen_code_expr(*stmt.expr);
    code_seq_concat(&ret, code_seq_singleton(code_lwr(3, SP, 0)));
    code_seq fp_seq = code_utils_compute_fp(4, stmt.idu->levelsOutward);
    code_seq_concat(&ret, fp_seq);
    unsigned int offset_count = id_use_get_attrs(stmt.idu)->offset_count;
    code_seq_add_to_end(&ret, code_swr(4, offset_count, 3));
    return ret;
}

//call statement
code_seq gen_code_call_stmt(call_stmt_t stmt){
    bail_with_error("Call statements are not supported in this version.");
    return code_seq_empty();
}

//block statement
code_seq gen_code_block_stmt(block_stmt_t stmt){
    return gen_code_block(*stmt.block);
}

//if statement
code_seq gen_code_if_stmt(if_stmt_t stmt){
    code_seq ret = gen_code_condition(stmt.condition);
    code_seq then_cs = gen_code_stmts(*stmt.then_stmts);
    code_seq else_cs = (stmt.else_stmts!=NULL) ? gen_code_stmts(*stmt.else_stmts) : code_seq_empty();

    unsigned int then_len = code_seq_size(then_cs);
    unsigned int else_len = code_seq_size(else_cs);

    code_seq_concat(&ret, code_seq_singleton(code_lwr(3, SP, 0)));
    code_seq_add_to_end(&ret, code_beq(3, 0, then_len + 1));
    code_seq_concat(&ret, then_cs);
    code_seq_add_to_end(&ret, code_jrel(else_len));
    code_seq_concat(&ret, else_cs);
    return ret;
}

//while statement
code_seq gen_code_while_stmt(while_stmt_t stmt){
    code_seq cond_cs = gen_code_condition(stmt.condition);
    code_seq body_cs = gen_code_stmts(*stmt.body);

    unsigned int cond_len = code_seq_size(cond_cs);
    unsigned int body_len = code_seq_size(body_cs);

    code_seq_concat(&cond_cs, code_seq_singleton(code_lwr(3, SP, 0)));
    code_seq_add_to_end(&cond_cs, code_beq(3, 0, body_len + 1));

    code_seq_concat(&cond_cs, body_cs);
    code_seq_add_to_end(&cond_cs, code_jrel(-(cond_len + body_len + 1)));
    return cond_cs;
}

//read statement
code_seq gen_code_read_stmt(read_stmt_t stmt){
    code_seq ret = code_seq_singleton(code_rch(3, 0));
    code_seq fp_seq = code_utils_compute_fp(4, stmt.idu->levelsOutward);
    code_seq_concat(&ret, fp_seq);
    unsigned int offset_count = id_use_get_attrs(stmt.idu)->offset_count;
    code_seq_add_to_end(&ret, code_swr(4, offset_count, 3));
    return ret;
}

//print statement
code_seq gen_code_print_stmt(print_stmt_t stmt){
    code_seq ret = code_seq_empty();
    code_seq expr_cs = gen_code_expr(stmt.expr);
    code_seq_concat(&ret, expr_cs);
  

    code_seq print_instr = code_seq_singleton(code_pint(SP, 0));
    code_seq_concat(&ret, print_instr);

    code_seq dealloc_cs = code_utils_deallocate_stack_space(1);
    code_seq_concat(&ret, dealloc_cs);

    return ret;
}

//conditionals
code_seq gen_code_condition(condition_t cond){
    switch (cond.cond_kind){
        case ck_db:
            return gen_code_db_condition(cond.data.db_cond);
        case ck_rel:
            return gen_code_rel_op_condition(cond.data.rel_op_cond);
        default:
            bail_with_error("error, unknown condition kind #: %d", cond.cond_kind);
    }
    return code_seq_empty();
}

//db conditions
code_seq gen_code_db_condition(db_condition_t cond){
    code_seq ret = gen_code_expr(cond.dividend);
    code_seq_concat(&ret, gen_code_expr(cond.divisor));
    code_seq_concat(&ret, code_seq_singleton(code_div(3, 0)));
    return ret;
}

//relational operator conditions
code_seq gen_code_rel_op_condition(rel_op_condition_t cond){
    code_seq ret = gen_code_expr(cond.expr1);
    code_seq_concat(&ret, gen_code_expr(cond.expr2));
    switch (cond.rel_op.code){
        case eqsym:
            code_seq_concat(&ret, code_seq_singleton(code_beq(3, 0, 1)));
            break;
        case neqsym:
            code_seq_concat(&ret, code_seq_singleton(code_bne(3, 0, 1)));
            break;
        case ltsym:
            code_seq_concat(&ret, code_seq_singleton(code_bltz(3, 0, 1)));
            break;
        case leqsym:
            code_seq_concat(&ret, code_seq_singleton(code_blez(3, 0, 1)));
            break;
        case gtsym:
            code_seq_concat(&ret, code_seq_singleton(code_bgtz(3, 0, 1)));
            break;
        case geqsym:
            code_seq_concat(&ret, code_seq_singleton(code_bgez(3, 0, 1)));
            break;
        default:
            bail_with_error("error, unknown opcode #: %d", cond.rel_op.code);
    }
    return ret;
}

//expressions
code_seq gen_code_expr(expr_t expr){
    switch (expr.expr_kind){
        case expr_bin:
            return gen_code_binary_op_expr(expr.data.binary);
        case expr_ident:
            return gen_code_ident(expr.data.ident);
        case expr_number:
            return gen_code_number(expr.data.number);
        case expr_negated:
            return gen_code_negated_expr(expr.data.negated);
        default:
            bail_with_error("error, unknown expression kind #: %d", expr.expr_kind);
    }
    return code_seq_empty();
}

//binary operators
code_seq gen_code_binary_op_expr(binary_op_expr_t expr){
    code_seq left_cs = gen_code_expr(*expr.expr1);
    code_seq right_cs = gen_code_expr(*expr.expr2);
    code_seq_concat(&left_cs, right_cs);
    code_seq op_cs = gen_code_op(expr.arith_op);
    code_seq_concat(&left_cs, op_cs);
    return left_cs;
}

//negate expressions
code_seq gen_code_negated_expr(negated_expr_t expr){
    code_seq ret = gen_code_expr(*expr.expr);
    code_seq_concat(&ret, code_seq_singleton(code_neg(3, 0, 3, 0))); // Negate $3
    return ret;
}

//identifiers
code_seq gen_code_ident(ident_t id){

    code_seq ret = code_utils_compute_fp(3, id.idu->levelsOutward);

    unsigned int offset_count = id_use_get_attrs(id.idu)->offset_count;

    code_seq_add_to_end(&ret, code_cpw(SP, 0, 3, offset_count));
    return ret;
}

//number literals
code_seq gen_code_number(number_t number){
    unsigned int offset = literal_table_lookup(number.text, number.value);
    code_seq ret = code_utils_allocate_stack_space(1);
    code_seq_add_to_end(&ret, code_cpw(SP, 0, GP, offset));
    return ret;
}

//operator
code_seq gen_code_op(token_t op){
    switch (op.code){
        case plussym:
        case minussym:
        case multsym:
        case divsym:
            return gen_code_arith_op(op);
        case eqsym:
        case neqsym:
        case ltsym:
        case leqsym:
        case gtsym:
        case geqsym:
            return gen_code_rel_op(op);
        default:
            bail_with_error("error, unknown opcode #: %d", op.code);
    }
    return code_seq_empty();
}

//arithmetic operator
code_seq gen_code_arith_op(token_t op){
    switch (op.code){
        case plussym:
            return code_seq_singleton(code_add(3, 0, 3, 0));
        case minussym:
            return code_seq_singleton(code_sub(3, 0, 3, 0));
        case multsym:
            return code_seq_singleton(code_mul(3, 0));
        case divsym:
            return code_seq_singleton(code_div(3, 0));
        default:
            bail_with_error("error, unknown opcode #: %d", op.code);
    }
    return code_seq_empty();
}

//relational operator
code_seq gen_code_rel_op(token_t op){
    switch (op.code){
        case eqsym:
            return code_seq_singleton(code_beq(3, 0, 1));
        case neqsym:
            return code_seq_singleton(code_bne(3, 0, 1));
        case ltsym:
            return code_seq_singleton(code_bltz(3, 0, 1));
        case leqsym:
            return code_seq_singleton(code_blez(3, 0, 1));
        case gtsym:
            return code_seq_singleton(code_bgtz(3, 0, 1));
        case geqsym:
            return code_seq_singleton(code_bgez(3, 0, 1));
        default:
            bail_with_error("error, unknown opcode #: %d", op.code);
    }
    return code_seq_empty();
}
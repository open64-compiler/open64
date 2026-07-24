/*
 * Test-only string tables for the focused tensor-fold contract fixture.
 * Keep the global string table and TCON character table separate so carrier
 * and inline dense bytes exercise Save_StrN/Index_to_char_array semantics.
 */

#include <string>
#include <vector>

#include "strtab.h"
#include "symtab_idx.h"
#include "targ_const.h"

static std::vector<std::string> DSL_tensor_fold_test_strtab;
static std::vector<std::string> DSL_tensor_fold_test_char_table;
static std::vector<TCON> DSL_tensor_fold_test_tcon_table;

void
Initialize_Strtab (UINT32)
{
    DSL_tensor_fold_test_strtab.clear();
    DSL_tensor_fold_test_strtab.push_back("");
    DSL_tensor_fold_test_char_table.clear();
    DSL_tensor_fold_test_char_table.push_back("");
    DSL_tensor_fold_test_tcon_table.clear();
    TCON zero;
    TCON_clear(zero);
    DSL_tensor_fold_test_tcon_table.push_back(zero);
}

void
Initialize_Strtab (const char *, UINT32 size)
{
    Initialize_Strtab(size);
}

STR_IDX
Save_Str (const char *str)
{
    if (DSL_tensor_fold_test_strtab.empty())
        Initialize_Strtab(1024);
    if (str == NULL)
        return STR_IDX_ZERO;

    DSL_tensor_fold_test_strtab.push_back(str);
    return DSL_tensor_fold_test_strtab.size() - 1;
}

UINT32
Save_StrN (const char *str, UINT32 size)
{
    if (DSL_tensor_fold_test_char_table.empty())
        Initialize_Strtab(1024);
    if (str == NULL || size == 0)
        return STR_IDX_ZERO;

    DSL_tensor_fold_test_char_table.push_back(std::string(str, size));
    return DSL_tensor_fold_test_char_table.size() - 1;
}

char *
Index_To_Str (STR_IDX idx)
{
    if (idx >= DSL_tensor_fold_test_strtab.size())
        return NULL;
    return const_cast<char *>(DSL_tensor_fold_test_strtab[idx].c_str());
}

char *
Index_to_char_array (UINT32 idx)
{
    if (idx >= DSL_tensor_fold_test_char_table.size())
        return NULL;
    return const_cast<char *>(DSL_tensor_fold_test_char_table[idx].data());
}

UINT32
Index_to_length (UINT32 idx)
{
    if (idx >= DSL_tensor_fold_test_char_table.size())
        return 0;
    return DSL_tensor_fold_test_char_table[idx].size();
}

STR_IDX
STR_Table_Size ()
{
    return DSL_tensor_fold_test_strtab.size();
}

TCON_IDX
Enter_tcon (const TCON& tcon)
{
    DSL_tensor_fold_test_tcon_table.push_back(tcon);
    return DSL_tensor_fold_test_tcon_table.size() - 1;
}

UINT32
TCON_Table_Size (void)
{
    return DSL_tensor_fold_test_tcon_table.size();
}

TCON
TCON_from_IDX (TCON_IDX tcon_idx)
{
    if (tcon_idx >= DSL_tensor_fold_test_tcon_table.size()) {
        TCON zero;
        TCON_clear(zero);
        return zero;
    }
    return DSL_tensor_fold_test_tcon_table[tcon_idx];
}

TCON
Host_To_Targ_String (TYPE_ID ctype, const char *bytes, UINT32 length)
{
    TCON result;
    TCON_clear(result);
    Set_TCON_ty(result, ctype);
    result.vals.sval.cp = Save_StrN(bytes, length);
    result.vals.sval.len = length;
    return result;
}

UINT32
TY_Table_Size (void)
{
    return 512;
}

BOOL
TY_is_tensor_extension (TY_IDX ty)
{
    return TY_IDX_index(ty) == 101 || TY_IDX_index(ty) == 201;
}

BOOL
TY_tensor_is_canonical (TY_IDX ty)
{
    return TY_is_tensor_extension(ty);
}

TY_IDX
TY_tensor_element_ty (TY_IDX)
{
    TY_IDX ty = TY_IDX_ZERO;
    Set_TY_IDX_index(ty, 301);
    return ty;
}

TYPE_ID
TY_mtype (TY_IDX ty)
{
    return TY_IDX_index(ty) == 301 ? MTYPE_I4 : MTYPE_UNKNOWN;
}

UINT64
TY_size (TY_IDX ty)
{
    return TY_IDX_index(ty) == 301 ? 4 : 0;
}

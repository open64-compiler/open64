/*
 * Test-only string tables for the focused tensor-fold contract fixture.
 * Keep the global string table and TCON character table separate so carrier
 * and inline dense bytes exercise Save_StrN/Index_to_char_array semantics.
 */

#include <string>
#include <vector>

#include "strtab.h"

static std::vector<std::string> DSL_tensor_fold_test_strtab;
static std::vector<std::string> DSL_tensor_fold_test_char_table;

void
Initialize_Strtab (UINT32)
{
    DSL_tensor_fold_test_strtab.clear();
    DSL_tensor_fold_test_strtab.push_back("");
    DSL_tensor_fold_test_char_table.clear();
    DSL_tensor_fold_test_char_table.push_back("");
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

STR_IDX
STR_Table_Size ()
{
    return DSL_tensor_fold_test_strtab.size();
}

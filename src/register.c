#include <R.h>
#include <Rinternals.h>
#include <stdlib.h> // for NULL
#include <R_ext/Rdynload.h>

/* FIXME: 
   Check these declarations against the C/Fortran source code.
*/

/* .Call calls */
extern SEXP bigmemory_CAttachFileBackedBigMatrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_CAttachSharedBigMatrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_CCleanIndices(SEXP, SEXP);
extern SEXP bigmemory_CCountLines(SEXP);
extern SEXP bigmemory_CDeepCopy(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_CGetNcol(SEXP);
extern SEXP bigmemory_CGetNrow(SEXP);
extern SEXP bigmemory_CGetType(SEXP);
extern SEXP bigmemory_CIsSubMatrix(SEXP);
extern SEXP bigmemory_CreateFileBackedBigMatrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_CreateLocalMatrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_CreateSharedMatrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_DirName(SEXP);
extern SEXP bigmemory_FileName(SEXP);
extern SEXP bigmemory_Flush(SEXP);
extern SEXP bigmemory_GetColOffset(SEXP);
extern SEXP bigmemory_GetColumnNamesBM(SEXP);
extern SEXP bigmemory_GetIndexColNames(SEXP, SEXP);
extern SEXP bigmemory_GetIndexRowNames(SEXP, SEXP);
extern SEXP bigmemory_GetIndivMatrixElements(SEXP, SEXP, SEXP);
extern SEXP bigmemory_GetIndivVectorMatrixElements(SEXP, SEXP);
extern SEXP bigmemory_GetMatrixAll(SEXP);
extern SEXP bigmemory_GetMatrixCols(SEXP, SEXP);
extern SEXP bigmemory_GetMatrixElements(SEXP, SEXP, SEXP);
extern SEXP bigmemory_GetMatrixRows(SEXP, SEXP);
extern SEXP bigmemory_GetMatrixSize(SEXP);
extern SEXP bigmemory_GetRowNamesBM(SEXP);
extern SEXP bigmemory_GetRowOffset(SEXP);
extern SEXP bigmemory_GetTotalColumns(SEXP);
extern SEXP bigmemory_GetTotalRows(SEXP);
extern SEXP bigmemory_GetTypeString(SEXP);
extern SEXP bigmemory_HasRowColNames(SEXP);
extern SEXP bigmemory_IsFileBackedBigMatrix(SEXP);
extern SEXP bigmemory_isnil(SEXP);
extern SEXP bigmemory_IsReadOnly(SEXP);
extern SEXP bigmemory_IsSeparated(SEXP);
extern SEXP bigmemory_IsShared(SEXP);
extern SEXP bigmemory_IsSharedMemoryBigMatrix(SEXP);
extern SEXP bigmemory_MWhichBigMatrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_MWhichRIntMatrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_MWhichRNumericMatrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_OrderBigMatrix(SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_OrderBigMatrixCols(SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_OrderRIntMatrix(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_OrderRIntMatrixCols(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_OrderRNumericMatrix(SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_OrderRNumericMatrixCols(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_ReadMatrix(SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_ReorderBigMatrix(SEXP, SEXP);
extern SEXP bigmemory_ReorderBigMatrixCols(SEXP, SEXP);
extern SEXP bigmemory_ReorderRIntMatrix(SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_ReorderRIntMatrixCols(SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_ReorderRNumericMatrix(SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_ReorderRNumericMatrixCols(SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_ReorderRRawMatrixCols(SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_SetColumnNames(SEXP, SEXP);
extern SEXP bigmemory_SetColumnOffsetInfo(SEXP, SEXP, SEXP);
extern SEXP bigmemory_SetIndivMatrixElements(SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_SetIndivVectorMatrixElements(SEXP, SEXP, SEXP);
extern SEXP bigmemory_SetMatrixAll(SEXP, SEXP);
extern SEXP bigmemory_SetMatrixCols(SEXP, SEXP, SEXP);
extern SEXP bigmemory_SetMatrixElements(SEXP, SEXP, SEXP, SEXP);
extern SEXP bigmemory_SetMatrixRows(SEXP, SEXP, SEXP);
extern SEXP bigmemory_SetRowNames(SEXP, SEXP);
extern SEXP bigmemory_SetRowOffsetInfo(SEXP, SEXP, SEXP);
extern SEXP bigmemory_SharedName(SEXP);
extern SEXP bigmemory_WriteMatrix(SEXP, SEXP, SEXP, SEXP, SEXP);

static const R_CallMethodDef CallEntries[] = {
    {"bigmemory_CAttachFileBackedBigMatrix",   (DL_FUNC) &bigmemory_CAttachFileBackedBigMatrix,   9},
    {"bigmemory_CAttachSharedBigMatrix",       (DL_FUNC) &bigmemory_CAttachSharedBigMatrix,       8},
    {"bigmemory_CCleanIndices",                (DL_FUNC) &bigmemory_CCleanIndices,                2},
    {"bigmemory_CCountLines",                  (DL_FUNC) &bigmemory_CCountLines,                  1},
    {"bigmemory_CDeepCopy",                    (DL_FUNC) &bigmemory_CDeepCopy,                    5},
    {"bigmemory_CGetNcol",                     (DL_FUNC) &bigmemory_CGetNcol,                     1},
    {"bigmemory_CGetNrow",                     (DL_FUNC) &bigmemory_CGetNrow,                     1},
    {"bigmemory_CGetType",                     (DL_FUNC) &bigmemory_CGetType,                     1},
    {"bigmemory_CIsSubMatrix",                 (DL_FUNC) &bigmemory_CIsSubMatrix,                 1},
    {"bigmemory_CreateFileBackedBigMatrix",    (DL_FUNC) &bigmemory_CreateFileBackedBigMatrix,    9},
    {"bigmemory_CreateLocalMatrix",            (DL_FUNC) &bigmemory_CreateLocalMatrix,            7},
    {"bigmemory_CreateSharedMatrix",           (DL_FUNC) &bigmemory_CreateSharedMatrix,           7},
    {"bigmemory_DirName",                      (DL_FUNC) &bigmemory_DirName,                      1},
    {"bigmemory_FileName",                     (DL_FUNC) &bigmemory_FileName,                     1},
    {"bigmemory_Flush",                        (DL_FUNC) &bigmemory_Flush,                        1},
    {"bigmemory_GetColOffset",                 (DL_FUNC) &bigmemory_GetColOffset,                 1},
    {"bigmemory_GetColumnNamesBM",             (DL_FUNC) &bigmemory_GetColumnNamesBM,             1},
    {"bigmemory_GetIndexColNames",             (DL_FUNC) &bigmemory_GetIndexColNames,             2},
    {"bigmemory_GetIndexRowNames",             (DL_FUNC) &bigmemory_GetIndexRowNames,             2},
    {"bigmemory_GetIndivMatrixElements",       (DL_FUNC) &bigmemory_GetIndivMatrixElements,       3},
    {"bigmemory_GetIndivVectorMatrixElements", (DL_FUNC) &bigmemory_GetIndivVectorMatrixElements, 2},
    {"bigmemory_GetMatrixAll",                 (DL_FUNC) &bigmemory_GetMatrixAll,                 1},
    {"bigmemory_GetMatrixCols",                (DL_FUNC) &bigmemory_GetMatrixCols,                2},
    {"bigmemory_GetMatrixElements",            (DL_FUNC) &bigmemory_GetMatrixElements,            3},
    {"bigmemory_GetMatrixRows",                (DL_FUNC) &bigmemory_GetMatrixRows,                2},
    {"bigmemory_GetMatrixSize",                (DL_FUNC) &bigmemory_GetMatrixSize,                1},
    {"bigmemory_GetRowNamesBM",                (DL_FUNC) &bigmemory_GetRowNamesBM,                1},
    {"bigmemory_GetRowOffset",                 (DL_FUNC) &bigmemory_GetRowOffset,                 1},
    {"bigmemory_GetTotalColumns",              (DL_FUNC) &bigmemory_GetTotalColumns,              1},
    {"bigmemory_GetTotalRows",                 (DL_FUNC) &bigmemory_GetTotalRows,                 1},
    {"bigmemory_GetTypeString",                (DL_FUNC) &bigmemory_GetTypeString,                1},
    {"bigmemory_HasRowColNames",               (DL_FUNC) &bigmemory_HasRowColNames,               1},
    {"bigmemory_IsFileBackedBigMatrix",        (DL_FUNC) &bigmemory_IsFileBackedBigMatrix,        1},
    {"bigmemory_isnil",                        (DL_FUNC) &bigmemory_isnil,                        1},
    {"bigmemory_IsReadOnly",                   (DL_FUNC) &bigmemory_IsReadOnly,                   1},
    {"bigmemory_IsSeparated",                  (DL_FUNC) &bigmemory_IsSeparated,                  1},
    {"bigmemory_IsShared",                     (DL_FUNC) &bigmemory_IsShared,                     1},
    {"bigmemory_IsSharedMemoryBigMatrix",      (DL_FUNC) &bigmemory_IsSharedMemoryBigMatrix,      1},
    {"bigmemory_MWhichBigMatrix",              (DL_FUNC) &bigmemory_MWhichBigMatrix,              7},
    {"bigmemory_MWhichRIntMatrix",             (DL_FUNC) &bigmemory_MWhichRIntMatrix,             8},
    {"bigmemory_MWhichRNumericMatrix",         (DL_FUNC) &bigmemory_MWhichRNumericMatrix,         8},
    {"bigmemory_OrderBigMatrix",               (DL_FUNC) &bigmemory_OrderBigMatrix,               4},
    {"bigmemory_OrderBigMatrixCols",           (DL_FUNC) &bigmemory_OrderBigMatrixCols,           4},
    {"bigmemory_OrderRIntMatrix",              (DL_FUNC) &bigmemory_OrderRIntMatrix,              5},
    {"bigmemory_OrderRIntMatrixCols",          (DL_FUNC) &bigmemory_OrderRIntMatrixCols,          6},
    {"bigmemory_OrderRNumericMatrix",          (DL_FUNC) &bigmemory_OrderRNumericMatrix,          5},
    {"bigmemory_OrderRNumericMatrixCols",      (DL_FUNC) &bigmemory_OrderRNumericMatrixCols,      6},
    {"bigmemory_ReadMatrix",                   (DL_FUNC) &bigmemory_ReadMatrix,                   8},
    {"bigmemory_ReorderBigMatrix",             (DL_FUNC) &bigmemory_ReorderBigMatrix,             2},
    {"bigmemory_ReorderBigMatrixCols",         (DL_FUNC) &bigmemory_ReorderBigMatrixCols,         2},
    {"bigmemory_ReorderRIntMatrix",            (DL_FUNC) &bigmemory_ReorderRIntMatrix,            4},
    {"bigmemory_ReorderRIntMatrixCols",        (DL_FUNC) &bigmemory_ReorderRIntMatrixCols,        4},
    {"bigmemory_ReorderRNumericMatrix",        (DL_FUNC) &bigmemory_ReorderRNumericMatrix,        4},
    {"bigmemory_ReorderRNumericMatrixCols",    (DL_FUNC) &bigmemory_ReorderRNumericMatrixCols,    4},
    {"bigmemory_ReorderRRawMatrixCols",        (DL_FUNC) &bigmemory_ReorderRRawMatrixCols,        4},
    {"bigmemory_SetColumnNames",               (DL_FUNC) &bigmemory_SetColumnNames,               2},
    {"bigmemory_SetColumnOffsetInfo",          (DL_FUNC) &bigmemory_SetColumnOffsetInfo,          3},
    {"bigmemory_SetIndivMatrixElements",       (DL_FUNC) &bigmemory_SetIndivMatrixElements,       4},
    {"bigmemory_SetIndivVectorMatrixElements", (DL_FUNC) &bigmemory_SetIndivVectorMatrixElements, 3},
    {"bigmemory_SetMatrixAll",                 (DL_FUNC) &bigmemory_SetMatrixAll,                 2},
    {"bigmemory_SetMatrixCols",                (DL_FUNC) &bigmemory_SetMatrixCols,                3},
    {"bigmemory_SetMatrixElements",            (DL_FUNC) &bigmemory_SetMatrixElements,            4},
    {"bigmemory_SetMatrixRows",                (DL_FUNC) &bigmemory_SetMatrixRows,                3},
    {"bigmemory_SetRowNames",                  (DL_FUNC) &bigmemory_SetRowNames,                  2},
    {"bigmemory_SetRowOffsetInfo",             (DL_FUNC) &bigmemory_SetRowOffsetInfo,             3},
    {"bigmemory_SharedName",                   (DL_FUNC) &bigmemory_SharedName,                   1},
    {"bigmemory_WriteMatrix",                  (DL_FUNC) &bigmemory_WriteMatrix,                  5},
    {NULL, NULL, 0}
};

void R_init_bigmemory(DllInfo *dll)
{
    R_registerRoutines(dll, NULL, CallEntries, NULL, NULL);
    R_useDynamicSymbols(dll, FALSE);
}

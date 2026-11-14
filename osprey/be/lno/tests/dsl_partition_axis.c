/* Copyright (C) 2026 Open64 Project */

/* Untagged scalar C discovery fixtures. No framework, tensor descriptors or
 * normalization operators supply axes. Design:
 * doc/AI-COMPILER-NATIVE-PARTITION-AXIS-v0.1.md. */
float source_rows[3][17], source_columns[17][3];
float sums[3], column_sums[3], squares[3], refused[3];
union { float input[3][17]; float output[3]; } overlapping;
float escaped_accumulator;
float equal_extents[3][3], equal_result[3];
float padded_source[3][18];

/* Each row owns one independently initialized ordered statistic. */
void native_row_sum(void)
{
  int instance, feature;
  for (instance = 0; instance < 3; ++instance) {
    float state = 0.0f;
    for (feature = 0; feature < 17; ++feature)
      state = state + source_rows[instance][feature];
    sums[instance] = state;
  }
}

/* The contributing axis is zero, not the physically trailing dimension. */
void native_column_sum(void)
{
  int unrelated_name, another_name;
  for (unrelated_name = 0; unrelated_name < 3; ++unrelated_name) {
    float temporary = 0.0f;
    for (another_name = 0; another_name < 17; ++another_name)
      temporary = temporary + source_columns[another_name][unrelated_name];
    column_sums[unrelated_name] = temporary;
  }
}

/* Squared contributors do not themselves imply an RMSNorm recipe. */
void native_squared_sum(void)
{
  int i, k;
  for (i = 0; i < 3; ++i) {
    float a = 0.0f;
    for (k = 0; k < 17; ++k)
      a = a + source_rows[i][k] * source_rows[i][k];
    squares[i] = a;
  }
}

/* Missing a rectangular static bound must not guess a feature cardinality. */
void native_dynamic_count(int count)
{
  int i, k;
  for (i = 0; i < 3; ++i) {
    float a = 0.0f;
    for (k = 0; k < count; ++k)
      a = a + source_rows[i][k];
    refused[i] = a;
  }
}

/* A sparse contribution domain is not full feature coverage. */
void native_stride_two(void)
{
  int i, k;
  for (i = 0; i < 3; ++i) {
    float a = 0.0f;
    for (k = 0; k < 17; k += 2)
      a = a + source_rows[i][k];
    refused[i] = a;
  }
}

/* Reusing one output location creates a cross-instance write conflict. */
void native_broadcast_store(void)
{
  int i, k;
  for (i = 0; i < 3; ++i) {
    float a = 0.0f;
    for (k = 0; k < 17; ++k)
      a = a + source_rows[i][k];
    refused[0] = a;
  }
}

/* An overlapping union must not be admitted from distinct field names. */
void native_alias_overlap(void)
{
  int i, k;
  for (i = 0; i < 3; ++i) {
    float a = 0.0f;
    for (k = 0; k < 17; ++k)
      a = a + overlapping.input[i][k];
    overlapping.output[i] = a;
  }
}

/* A product of different elements is not a squared-input statistic. */
void native_not_square(void)
{
  int i, k;
  for (i = 0; i < 3; ++i) {
    float a = 0.0f;
    for (k = 0; k < 17; ++k)
      a = a + source_rows[i][k] * source_columns[k][i];
    refused[i] = a;
  }
}

/* Global observable recurrence state is outside the first local contract. */
void native_escaping_state(void)
{
  int i, k;
  for (i = 0; i < 3; ++i) {
    escaped_accumulator = 0.0f;
    for (k = 0; k < 17; ++k)
      escaped_accumulator = escaped_accumulator + source_rows[i][k];
    refused[i] = escaped_accumulator;
  }
}

/* Equal extents prevent dimension-size heuristics from identifying the axes. */
void native_equal_extents(void)
{
  int feature, instance;
  for (feature = 0; feature < 3; ++feature) {
    float renamed = 0.0f;
    for (instance = 0; instance < 3; ++instance)
      renamed = renamed + equal_extents[feature][instance];
    equal_result[feature] = renamed;
  }
}

/* PREOPT may canonicalize a pure local temporary; discovery does not need a
 * second expression-substitution engine to accept that canonical result. */
void native_temporary(void)
{
  int i, k;
  for (i = 0; i < 3; ++i) {
    float a = 0.0f;
    for (k = 0; k < 17; ++k) {
      float term = source_rows[i][k];
      a = a + term;
    }
    refused[i] = a;
  }
}

/* Full logical feature coverage is not proved by a shorter reduction bound. */
void native_partial_coverage(void)
{
  int i, k;
  for (i = 0; i < 3; ++i) {
    float a = 0.0f;
    for (k = 0; k < 17; ++k)
      a = a + padded_source[i][k];
    refused[i] = a;
  }
}

/* Predicated contributors need a separately designed domain proof. */
void native_conditional(void)
{
  int i, k;
  for (i = 0; i < 3; ++i) {
    float a = 0.0f;
    for (k = 0; k < 17; ++k)
      if (source_rows[i][k] > 0.0f)
        a = a + source_rows[i][k];
    refused[i] = a;
  }
}

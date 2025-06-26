# Online 2SpamH

**Online 2SpamH** is the second version of [2SpamH](https://github.com/sambanlab/TwoSpamH), a two-stage pre-processing algorithm for passively sensed mobile health (mHealth) data. The original method is described in this [paper](https://pmc.ncbi.nlm.nih.gov/articles/PMC11548539/).

The method employs a **self-supervised approach** to infer missing data labels for passively collected sensor data using auxiliary device usage information. Key improvements in **Online 2SpamH** include:

- **Automated thresholding**: No need to manually define "low" and "high" levels of device usage or sensor activity.
- **Online implementation**: Supports real-time data processing as new data is collected.
- **Confidence scoring**: Quantifies the confidence of each "Missing"/"Non-missing" label.
- **Improved usability**: Updated variable naming and visualizations for easier interpretation and integration.

**Manuscript**: *COMING SOON*

## Tutorial

### `TwoSpamH_train()`

To label all existing observations in an input dataset as "Missing" or "Non-missing", use the `TwoSpamH_train` function:

```
Online2SpamH::TwoSpamH_train(
  data = data, 
  passive_variable = "step_count", 
  phone_usage_vars = c("battery_var", "screen_unlocks", "device_notification"), 
  activity_level_vars = "num_uploads",
  plot.data = T,
  conf = F
)
```
**Inputs**:
* `data` - Data frame with auxiliary device usage information of a single user aggregated over a pre-specified time interval (e.g., daily). The example dataset can be accessed from `data/example_data.rda`.
* `passive_variable` - Name of a variable (column) in the data representing a passively collected variable to be labeled as "Missing" or "Non-missing" (e.g., step count).
* `phone_usage_vars` - Name (or vector of names) of variables in the data representing device usage (e.g., battery variance, screen unlocks, device notifications).
* `activity_level_vars` - Name (or vector of names) of variables in the data representing sensor activity (e.g., number of uploads to the server)
* `plot.data` - Boolean, whether to show the plot with labeled data.
* `conf` - Boolean, whether to calculate the confidence of "Missing"/"Non-missing" label assignment.

**Output**: The original data frame with `label` column representing "Missing"/"Non-missing" label.

### `Online_TwoSpamH()`

To label a new observation as "Missing" or "Non-missing", use the `Online_TwoSpamH` function:
```
Online2SpamH::Online_TwoSpamH(
  new_data = new_observation,
  training_data = training_data,
  passive_variable = "step_count", 
  phone_usage_vars = c("battery_var", "screen_unlocks", "device_notification"), 
  activity_level_vars = "num_uploads",
  plot.data = T
)

```
**Inputs**:
* `new_data` - Data frame with a single new observation to be labeled as "Missing" or "Non-missing".
* `training_data` - Output of `TwoSpamH_train()` function (data frame with "Missing"/"Non-missing" labels).
* `passive_variable` - Name of a variable (column) in `new_data` representing a passively collected variable to be labeled as "Missing" or "Non-missing" (e.g., step count).
* `phone_usage_vars` - Name (or vector of names) of variables in `new_data` representing device usage (e.g., battery variance, screen unlocks, device notifications).
* `activity_level_vars` - Name (or vector of names) of variables in `new_data` representing sensor activity (e.g., number of uploads to the server)
* `plot.data` - Boolean, whether to show the plot with labeled data and highlighted new observation.

**Output**: The new observation `new_data` data frame with `label` column representing "Missing"/"Non-missing" label.

### Full Example

```
data <- Online2SpamH::example_data

training_data <- Online2SpamH::TwoSpamH_train(
  data = data, 
  passive_variable = "step_count", 
  phone_usage_vars = c("battery_var", "screen_unlocks", "device_notification"), 
  activity_level_vars = "num_uploads",
  plot.data = F,
  conf = F
)

new_observation <- data.frame(
  battery_var = 0.09,
  screen_unlocks = 40,
  device_notification = 40, 
  num_uploads = 80,
  step_count = 6000)

Online2SpamH::Online_TwoSpamH(
  new_data = new_observation,
  training_data = training_data,
  passive_variable = "step_count", 
  phone_usage_vars = c("battery_var", "screen_unlocks", "device_notification"), 
  activity_level_vars = "num_uploads",
  plot.data = T
)

```
---

### `TwoSpamH()`

Original 2SpamH function with updated variable naming and visualizations; requires the choice of thresholds for "low" and "high" phone usage and sensor activity.

```
Online2SpamH::TwoSpamH(
  data = data, 
  passive_variable = passive_variable, 
  phone_usage_vars = phone_usage_vars, 
  activity_level_vars = activity_level_vars,
  thresholds = data.frame( 
    lower_bound_phone_usage = 0.2, 
    upper_bound_phone_usage = 0.8, 
    lower_bound_activity_level = 0.3,
    upper_bound_activity_level = 0.7),
  plot.data = T
)
```

**Inputs**:
* `data` - Data frame with auxiliary device usage information of a single user aggregated over a pre-specified time interval (e.g., daily). The example dataset can be accessed from `data/example_data.rda`.
* `passive_variable` - Name of a variable (column) in the data representing a passively collected variable to be labeled as "Missing" or "Non-missing" (e.g., step count).
* `phone_usage_vars` - Name (or vector of names) of variables in the data representing device usage (e.g., battery variance, screen unlocks, device notifications).
* `activity_level_vars` - Name (or vector of names) of variables in the data representing sensor activity (e.g., number of uploads to the server)
* `thresholds` - Data frame representing lower and upper bounds for "low" and "high" phone usage and sensor activity with the following format and default values: `data.frame(lower_bound_phone_usage = 0.3, upper_bound_phone_usage = 0.7, lower_bound_activity_level = 0.3, upper_bound_activity_level = 0.7)`.
* `plot.data` - Boolean, whether to show the plot with labeled data.

**Output**: The original data frame with `label` column representing "Missing"/"Non-missing" label.

---
Please contact seb4012@med.cornell.edu with any questions.

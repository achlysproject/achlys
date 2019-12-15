## Achlys usage examples

### Estimation of Pi
The following [Code](https://github.com/achlysproject/achlys/blob/master/resources/examples/PI.md) shows how
Achlys can be used to perform approximations of the value of π. The approximation becomes more accurate as the number
participating nodes increases. The entire computation can be declared in a single Achlys Task using the `achlys:declare/4`
function. Then, it can be propagated in a cluster using `achlys:bite/1` with the variable containing
the task given as a parameter.

### Adding a `gen_server` to provide tasks automatically
Achlys can be easily extended with a new application-specific server that acts as a [Task Provider](https://github.com/achlysproject/achlys/blob/master/resources/TASK_MODEL.md)
that can automatically interact with the Achlys Task model as demonstrated in the `achlys_task_provider.erl` example
module.


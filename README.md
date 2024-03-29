# Logic Programming for Data Lineage Modelling

Example of using Clojure `core.logic` logic programming to model data
lineage, or how different sources of data are used by, flowing through
and transformed by an application. 

This is also relevant for supply chains.

We are interested in solving problems such as these:

- Which of our sandwiches are based on _Triticum dicoccum_ flour?
- The meat we got from Romania was horse meat, not beef. Which
  of our food products should we recall?
- Our supplier of real estate information had a bug that reported
  incorrect sizes of summer houses in August. Did we grant any
  loans based on this incorrect information?
- We found a bug in our aggregation model, did we send out any
  compliance-violating letters to customers due to the downstream
  impact of bad data points from this bug?


## Usage

Just a workbook with examples. Use it for inspiration. Submit a pull
request if you have a better way to do things.

## License

Copyright © 2019 Martin Jul

This program and the accompanying materials are made available under the
terms of the MIT License. See the `LICENSE` file for detailed information.

# backtest
R scripts for backtesting momentum strategies

# Try it out

## via Docker

Install pre-requisites
- make
- docker

```bash
git clone https://github.com/jschaub30/backtest
cd backtest
make build
make run
```

To view the resulting plots in an interactive web page
```
cd html
python -m http.server 8080
```
...then open your browser to http://localhost:8080/

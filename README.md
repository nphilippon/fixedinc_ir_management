# Fixed Income Interest Rate Management Dashboard

The purpose of this app is to assist a portfolio manager in understanding the exposure and risk of a fixed income portfolio.

The first page features charts displaying details on the macro environment including historical yields for US treasury bonds, forward interest rate curves extrapolated using a cubic-spline model, as well as duration and delta.

The second page features a portfolio builder that a PM can use to input their bond portfolios and store them locally or via github.

Lastly the risk management tab attempts to explain exposure and risk with Portfolio Value over time. Under development are greek exposure approximations and risk metrics.

Docker Instructions:

docker pull alexhirtle/fixedinc_ir_management:latest

docker run -d -p 3838:3838 alexhirtle/fixedinc_ir_management:latest

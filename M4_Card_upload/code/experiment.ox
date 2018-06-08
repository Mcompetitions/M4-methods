#include <oxstd.oxh>
#include <arma.oxh>
#include <oxdraw.oxh>
#include <oxfloat.oxh>

#import "ForecastAB"

replicate(const sLabel, const aArgs)
{
	decl as = new array[sizeof(aArgs)], i, s;
	foreach (s in aArgs[i])
		as[i] = replace(sLabel, "%s", s);
	return as;
}

main()
{
	decl model = new ForecastAB();

	model.Load("../data/Hourly_01.in7");
//	model.Load("../data/Daily_01.in7");
//	model.Load("../data/Monthly_09.in7");
//	model.Load("../data/Monthly_06.in7");
//	model.Load("../data/Yearly_02.in7");
//	model.Load("../data/Yearly_03.in7");
//	model.Load("../data/Yearly_01.in7");
//	model.Load("../data/Quarterly_02.in7");
//	model.Load("../data/Yearly_01.in7");
//	model.Load("../data_M3/Monthly_01.in7");

//	model.Load("../data_M3/Yearly_01.in7");
//	model.Select("Y", {"Y113","Y8","Y157","Y156","Y115"});

//	model.Select("Y", {/*"H264",*/"H181"});
//	model.Select("Y", {"D2178"}); // daily_01
//	model.Select("Y", {"M41906"}); // monthly_04
//	model.Select("Y", {"Y19545","Y19538","Y19557"}); // Yearly_03
//	model.Select("Y", {"Y3066"}); // Yearly_05
//	model.Select("Y", {"Y14067", "Y14123"}); // Yearly_03
//	model.SetVar(cumulate(model.GetVar("Y760")), "CY760");
//	model.Select("Y", {"CY760"});
//	model.Select("Y", {"W340","W323","W9","W294"});
//	model.SetVar(diff(model.GetVar("Q22933"), model.GetFrequency()), "DsQ22933");
//	model.SetVar(diff(model.GetVar("Q5627"), model.GetFrequency()), "DsQ5627");
//	M4::TransformDb(model, "DS");
//	model.Select("Y", {"Q17261"});
	model.Select("Y", {"H10","H100","H200","H300"});


decl ch = 48, freq = model.GetFrequency();

	model.SetHoldBack(ch);
	model.SetLimit(5040);
	
	model.Report();
	decl time = timer();
	
	decl asnaive = {"naive2"};
	model.AddForecasts(ch, asnaive, model.ForecastNaive);
	decl asmm = {"Delta", "Rho", "Card"};
	model.AddForecasts(ch, asmm,   model.Forecast);

	println("done in ", timespan(time));

	decl asmethods = asnaive ~ asmm;
	decl mape = model.Collect("mase", asmethods);
	decl aslabel = replicate("MASE(%s)", asmethods);

	print("%r", model.Collect("names"), "%c", aslabel, "%13.3f", mape');

	print("%r", model.Collect("names"), "%c", aslabel, "%13.3f", mape' ./ mape[0][]');
}

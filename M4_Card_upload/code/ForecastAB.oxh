/*--------------------------------------------------------------------------
 * forecast.h - definitions/declarations for ForecastAB class
 *
 *       (C) Jurgen Doornik 2018-
 *
 *--------------------------------------------------------------------------*/

#ifndef FORECAST_AB_INCLUDED
#define FORECAST_AB_INCLUDED

#import <modelbase>

struct VarForc
{
	VarForc(const sY, const iT0, const iT1, const iT2, const iFreq);
	decl name;					///< name of variable
	decl t0;					///< database index of first valid observation for sample which may have gaps
	decl t1;					///< start of trailing contiguous block used for estimation
	decl t2;					///< database index of last estimation observation, forecasting starts at t2 + 1;
	decl t0a;					///< database index of first valid observation
	decl t1a;					///< start of full trailing contiguous block
	decl t2a;					///< database index of last valid observation, forecasting starts at t2 + 1;
	decl type;					///< variable type
	decl freq;					///< seasonal frequency (can be different from database specification)
	decl freq2;					///< secondary frequency (freq*freq2)
	decl min;					///< minimum value
	decl max;					///< maximum value
	decl aproperties;			///< array of preperties

	decl aforc;					///< array[cF] with [cH_f][1] matrices with forecasts
	decl aforc_lu;				///< array[cF] with [cH_f][2] matrices with forecast lower and upper bounds (or <>)
	decl aforc_info;			///< array[cF] matrix[7][1] start of forecast, MAPE
	decl asforc;				///< array[cF] of forecast descriptions
};

class ForecastAB : Modelbase
{
	decl m_asY;					///< names of variables	array[m_cY]
	decl m_aVarForc;			///< VarForc values [1][m_cY]
	decl m_cHoldback;			///< holdback for forecast evaluation
	decl m_cCutback;			///< cutback for sample reduction (ignored entirely)
	decl m_cLimit;				///< limit of sample size used for forecasting

    ForecastAB();
	~ForecastAB();
	Init();
	Clear();
	virtual GetPackageName();
	virtual GetPackageVersion();
	GetVarNames();
	virtual IsUnivariate();
	SetHoldBack(const cHB);
	SetCutBack(const cCB);
	SetLimit(const cT);
	static SeasonalityTests(const vX, const iFreq);
	static SeasonalityTest(const vX, const iFreq);
	static HasSeasonality(const vX, const iFreq, const dAlpha=0.1);
	static SeasonalAnova(const vX, const iFreq);
	static MeanTest(const vX);
	static Accuracy(const vForc, const vY, const vYest, const iFreq);
	static MSIS(const vForcLo, const vForcHi, const vY, const vYest, const iFreq, const dAlpha=0.05);
	static IntervalAccuracy(const vForcLo, const vForcHi, const vY, const vYest, const iFreq);
	static MA(const vX, const sType, const vPar);
	static Decompose(const vY, const iFreq, const sType);
	virtual ForecastNaive(const vY, const iFreq, const cH, const sType, const sY="");
	virtual InitData();
	static ForecastDelta(const vY, const iFreq, const cH, const sType="", const sY="");
	static ForecastRho(const vY, const iFreq, const cH, const sType="", const sY="");
	static SeasonalDummies(const cT, const iFreq, const iFreq2=1, const cH=0);
	static Calibrate(const vY, const iFreq, const cH, const sType="", const sY="");
	static SeasonalInfo(const vY, iFreq, const bCheckFreq=FALSE);
	virtual Forecast(const vY, const iFreq, const cH, const sType, const sY);
	AddForecasts(const cH, const sType, const fnForc);
	CollectForecasts(const sType);
	CollectIt(const fnGetIt, const cRows=1);
	Collect(const sType, const sWhere="");

	virtual Report();

protected:	
	/** Type of variable. */
	enum
	{	TYPE_EMPTY, TYPE_VAR, TYPE_DETERMINISTIC
	}

public:	
	/** variable types for model specification */
	enum
	{   Y_VAR,  X_VAR
		 
	};
	/** estimators */
	enum
	{   M_NONE,  M_OLS
	};
};

#endif /* FORECAST_AB_INCLUDED */

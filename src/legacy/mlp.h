/*
 * Autogenerated Neural Network Code
 * The network expects an array of 5 unnormalized floats
 * as input to MultilayerPerceptron::ProcessInput(float *inputs)
 * inputs[0] = absolute 3-momentum of photon system (=abs(p1+p2))
 * inputs[1] = invariant mass of photon system
 * inputs[2] = rapidity of photon system
 * inputs[3] = energy separation of the photons (=E_photon1 - E_photon2) (positive)
 * inputs[4] = rapidity difference of the photons (=eta_photon1 - eta_photon2)
 * photon1 is the leading photon
 */

#ifndef _NEURAL_NETWORK_2089295372
#define _NEURAL_NETWORK_2089295372
class MultilayerPerceptron{
protected:
	int _numInputs;
	int _numOutputs;
	int _numHiddenLayers;
	int _numHiddenNeurons;
	double _input[6];
	double _hidden[1][6];
	double _output[1];
	double _inputWeights[5][6];
	double _outputWeights[1][6];
	double _min[5];
	double _max[5];
	double Clamp(double val, int i){
		return (val - _min[i])/(_max[i]-_min[i]);
	}
	double ActivationFunction(double x){
		return 1.0/(1.0 + exp(-x));
	}
public:
	MultilayerPerceptron(){
		_numInputs = 5;
		_numOutputs = 1;
		_numHiddenLayers = 1;
		_numHiddenNeurons = 5;
		_inputWeights[0][0] = -26.5436436208756;
		_inputWeights[0][1] = -105.201926315295;
		_inputWeights[0][2] = 24.6073124912745;
		_inputWeights[0][3] = -1.68267883584783;
		_inputWeights[0][4] = -9.92840363252831;
		_inputWeights[0][5] = -5.35677046875259;
		_inputWeights[1][0] = -1.98989424736827;
		_inputWeights[1][1] = 14.4910862405003;
		_inputWeights[1][2] = -6.30057969701444;
		_inputWeights[1][3] = 6.91241342781397;
		_inputWeights[1][4] = -51.7353079829265;
		_inputWeights[1][5] = 6.34869225002198;
		_inputWeights[2][0] = 14.1020530984119;
		_inputWeights[2][1] = -259.040736622664;
		_inputWeights[2][2] = 6.54087418714816;
		_inputWeights[2][3] = -3.83259668602378;
		_inputWeights[2][4] = 15.1884835565013;
		_inputWeights[2][5] = 2.83629429606814;
		_inputWeights[3][0] = -3.13796952331714;
		_inputWeights[3][1] = -515.668238535109;
		_inputWeights[3][2] = -3.00991154071035;
		_inputWeights[3][3] = 2.09689105982024;
		_inputWeights[3][4] = -3.06335817980597;
		_inputWeights[3][5] = 25.8081366883461;
		_inputWeights[4][0] = 4.41450965377739;
		_inputWeights[4][1] = 313.338751969711;
		_inputWeights[4][2] = 2.30228912051236;
		_inputWeights[4][3] = -1.32915373957745;
		_inputWeights[4][4] = 3.1366432835502;
		_inputWeights[4][5] = -16.8858196401603;
		_outputWeights[0][0] = -5.76471100567132;
		_outputWeights[0][1] = 11.7243229474467;
		_outputWeights[0][2] = -10.2015289149802;
		_outputWeights[0][3] = -16.2346222311754;
		_outputWeights[0][4] = 10.2779552048887;
		_outputWeights[0][5] = 11.6212218868151;
		_min[0] = 0.00186061859130859;
		_max[0] = 1829.97106933594;
		_min[1] = 50.0379072297931;
		_max[1] = 1401.85227779932;
		_min[2] = -2.48713931977048;
		_max[2] = 2.48885551966595;
		_min[3] = -1024.10540771484;
		_max[3] = -0.000173568725585938;
		_min[4] = -4.75166392314343;
		_max[4] = 4.7086572377491;
	}
	int GetNumInputs(void){
		return _numInputs;
	}
	int GetNumOutputs(void){
		return _numOutputs;
	}
	int GetNumHiddenLayers(void){
		return _numHiddenLayers;
	}
	int GetNumHiddenNeurons(void){
		return _numHiddenNeurons;
	}
	void ProcessInput(double *inputs){
		_input[_numInputs] = 1.0;
		for(int i=0;i<_numHiddenLayers;i++){
			_hidden[i][_numHiddenNeurons] = 1.0;
		}
		for(int i=0;i<_numInputs;i++){
			_input[i] = Clamp(inputs[i],i);
		}
		for(int n=0;n<_numHiddenNeurons;n++){
			double sum = 0.0;
			for(int i=0;i<(_numInputs+1);i++){
				sum += _input[i] * _inputWeights[n][i];
			}
			_hidden[0][n] = ActivationFunction(sum);
		}
		for(int n=0;n<_numOutputs;n++){
			double sum = 0.0;
			for(int i=0;i<(_numHiddenNeurons+1);i++){
				sum += _outputWeights[n][i] * _hidden[_numHiddenLayers-1][i];
			}
			_output[n] = ActivationFunction(sum);
		}
	}
	double GetOutput(int i){
		return _output[i];
	}
};
#endif

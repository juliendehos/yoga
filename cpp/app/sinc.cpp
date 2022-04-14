#include <yoga/Random.hpp>

#include <iostream>
#include <torch/torch.h>

const size_t batchSize = 100;
const size_t trainSize = 1000;
const size_t predSize = 200;
const size_t nHidden = 100;
const size_t nInput = 1;
const size_t nOutput = 1;
const size_t nEpoch = 10000;
const size_t nEpochPrint = 100;

using DataVec = std::vector<std::pair<float, float>>;

float sinc(float x) {
    return fabs(x)<0.001 ? 1. : sin(x)/x;
}

/*
struct NetworkImpl : torch::nn::SequentialImpl {
  NetworkImpl() {
    using namespace torch::nn;
    push_back(Linear(nInput, nHidden));
    push_back(Functional(torch::tanh));
    push_back(Linear(nHidden, nHidden));
    push_back(Functional(torch::tanh));
    push_back(Linear(nHidden, nOutput));
  }
};
TORCH_MODULE(Network);
*/


struct NetworkImpl : torch::nn::Module {

    NetworkImpl() :
        fc1(nInput, nHidden),
        fc2(nHidden, nHidden),
        fc3(nHidden, nOutput)
    {
        register_module("fc1", fc1);
        register_module("fc2", fc2);
        register_module("fc3", fc3);
    }

    torch::Tensor forward(torch::Tensor x) {
        x = torch::tanh(fc1(x));
        x = torch::tanh(fc2(x));
        x = fc3(x);
        return x;
    }
    
    torch::nn::Linear fc1, fc2, fc3;

};
TORCH_MODULE(Network);

class CustomDataset : public torch::data::datasets::Dataset<CustomDataset> {
    using example_t = torch::data::Example<>;

    std::vector<example_t> _data;

    public:
    CustomDataset(const DataVec & dataVec, torch::Device & device) {
        for (const auto xy : dataVec) {
            auto x = torch::empty({1});
            auto y = torch::empty({1});
            x[0] = xy.first;
            y[0] = xy.second;
            x = x.to(device);
            y = y.to(device);
            _data.push_back({std::move(x), std::move(y)});
        }
    }

    example_t get(size_t index) {
        return _data[index];
    }

    torch::optional<size_t> size() const {
        return _data.size();
    }
};

int main() {

    // generate input data (noisy sinc)
    Random xRng({});
    Random noiseRng({});
    DataVec trainVec(trainSize);
    for (auto & xy : trainVec) {
        const float x = xRng.uniformDouble(-40, 40);
        const float y = sinc(x) + noiseRng.normalDouble(0, 0.02);
        xy = {x, y};
    };

    // initialize torch
    torch::DeviceType deviceType;
    if (torch::cuda::is_available()) {
        std::cout << "CUDA available! Training on GPU." << std::endl;
        deviceType = torch::kCUDA;
    } else {
        std::cout << "Training on CPU." << std::endl;
        deviceType = torch::kCPU;
    }
    torch::Device device(deviceType);

    // dataloader
    using sampler_t = torch::data::samplers::RandomSampler;
    auto trainSet =
        CustomDataset(trainVec, device).map(torch::data::transforms::Stack<>());
    auto trainLoader =
        torch::data::make_data_loader<sampler_t>(std::move(trainSet), batchSize);
    const float nBatchF = 1 + int((trainSize - 1) / float(batchSize));

    // nn
    Network network;
    std::cout << network << std::endl;
    network->to(device);

    torch::optim::Adam optim(network->parameters());
    auto lossFunc = torch::nn::MSELoss();
    std::vector<std::tuple<int, float>> stats;

    for (int iEpoch=0; iEpoch<nEpoch; ++iEpoch) {
        network->train();
        float sumLoss = 0;
        for (auto& batch : *trainLoader) {
            auto data = batch.data;
            auto target = batch.target;
            auto output = network->forward(data);
            auto loss = lossFunc(output, target);

            optim.zero_grad();
            loss.sum().backward();
            optim.step();

            sumLoss += loss.item<float>();
        }

        if (iEpoch % nEpochPrint == 0) {
            const float avgLoss = sumLoss / nBatchF;
            std::cout 
                << "epoch " << iEpoch << ", "
                << "loss " << avgLoss << std::endl;
            stats.push_back({iEpoch, avgLoss});
            torch::save(network, "network.pt");
        }

        // TODO test/validation ?

    }

    // prediction (on CPU)
    network->to(torch::kCPU);
    network->eval();
    DataVec predVec(predSize);
    auto xPredTensor = torch::linspace(-50, 50, predSize);
    auto pred = torch::empty({1});
    auto accessX = xPredTensor.accessor<float, 1>();
    for (int i=0; i<predSize; ++i) {
        pred[0] = xPredTensor[i];
        pred = network->forward(pred);
        predVec[i] = {accessX[i], pred.item<float>()};
    }

    // ground truth
    DataVec gtVec;
    for (double x=-50; x<50; x+=0.5) {
        const double y = sinc(x);
        gtVec.push_back({x, y});
    }

    /*
    // plot func
    {
        Gnuplot gnuplot;
        gnuplot
            << "set terminal svg size 800,400 \n"
            << "set output 'out-sinc-func.svg' \n"
            << "set grid xtics ytics \n"
            << "set xlabel 'x' \n"
            << "set ylabel 'sinc(x) + epsilon' \n"
            << "set xrange [-50:50] \n"
            << "set yrange [-0.3:1] \n"
            << "plot "
            << gnuplot.file1d(gtVec) 
            << " title 'ground truth' with lines linewidth 2 linecolor rgb 'green', "
            << gnuplot.file1d(predVec) 
            << " title 'prediction' with lines linewidth 2 linecolor rgb 'red', "
            << gnuplot.file1d(trainVec) 
            << " title 'train set' pointtype 7 pointsize 0.2 linecolor rgb 'blue', "
            << std::endl;
    }

    // plot loss
    {
        Gnuplot gnuplot;
        gnuplot
            << "set terminal svg size 800,400 \n"
            << "set output 'out-sinc-loss.svg' \n"
            << "set grid xtics ytics \n"
            << "set xlabel 'epoch' \n"
            << "set ylabel 'loss' \n"
            << "set logscale y \n"
            << "plot "
            << gnuplot.file1d(stats) 
            << " using 1:2 notitle with lines linewidth 2 linecolor rgb 'red', "
            << std::endl;
    }
    */

    return 0;
}



